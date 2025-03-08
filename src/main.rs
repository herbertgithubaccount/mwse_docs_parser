#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![feature(yeet_expr)]
#![feature(try_trait_v2_yeet)]
#![feature(type_alias_impl_trait)]
#![feature(ptr_as_ref_unchecked)]
#![feature(never_type)]
#![feature(str_as_str)]

mod lex;

mod parse;
mod writer;
pub mod package;
pub mod error;
use std::{collections::{HashMap, HashSet}, path::{Path, PathBuf}, pin::Pin, sync::Arc};

use derive_more::{Display, From};
use error::Error;
use lex::LexError;
use log::{debug, error, info, trace, warn};
use package::{ClassPkg, EPkg, EventPkg, Example, FunctionPkg, LibPkg, MethodPkg, OperatorPkg, PkgCore, ValuePkg};
pub use parse::*;
use tokio::task::JoinHandle;
// use writer::Writable;


// struct DirPackage

// struct DirDocs {
// 	packages: Vec<Package>,
// }

#[derive(Debug, Default)]
#[repr(C)]
pub struct DirPackages {
	classes: Vec<ClassPkg>,
	functions: Vec<FunctionPkg>,
	methods: Vec<MethodPkg>,
	values: Vec<ValuePkg>,
	libs: Vec<LibPkg>,
	events: Vec<EventPkg>,
	operators: Vec<OperatorPkg>,
}

impl DirPackages {
	fn add_pkg(&mut self, epkg: EPkg) {
		// TODO: can this be done in a branchless manner?
		match epkg {
			EPkg::Class(pkg) => self.classes.push(pkg),
			EPkg::Function(pkg) => self.functions.push(pkg),
			EPkg::Method(pkg) => self.methods.push(pkg),
			EPkg::Value(pkg) => self.values.push(pkg),
			EPkg::Lib(pkg) => self.libs.push(pkg),
			EPkg::Event(pkg) => self.events.push(pkg),
			EPkg::Operator(pkg) => self.operators.push(pkg),
		}
	}
	fn extend(&mut self, other: DirPackages) {
		self.classes.extend(other.classes);
		self.functions.extend(other.functions);
		self.methods.extend(other.methods);
		self.values.extend(other.values);
		self.libs.extend(other.libs);
		self.events.extend(other.events);
		self.operators.extend(other.operators);
	}

	fn total_len(&self) -> usize {
		  self.classes.len() 
		+ self.functions.len() 
		+ self.methods.len() 
		+ self.values.len() 
		+ self.libs.len() 
		+ self.events.len() 
		+ self.operators.len() 
	}
}

/// Recursively processes a file and builds a `EPkg` from it.
/// If there is a directory with the same name as `path`, then that directory will be recursively processed
/// and any packages found there will be added to the package returned by this function.
/// The `Box::pin` stuff is necessary for recursive async functions.
fn process_file(path: PathBuf) -> Pin<Box<dyn Send + Sync + Future<Output = Result<EPkg, Error>>>> {
	Box::pin(async move {
			
		// Only return a file if we were able to parse it.
		// Log the errors instead of killing the whole program.
		let mut epkg = match EPkg::parse_from_file(&path).await {
			Err(e) => {
				error!("{path:?}: {e:?}");
				return Err(e);
			},
			Ok(epkg) => epkg,
		};


		// Directory is what you get after removing the `.lua` extension from the filepath.
		let dir = path.with_extension("");

		// Recursively process the child directory if necessary
		if dir.exists() && dir.is_dir() {

			// process_dir(dir, ignore).await?
			trace!("processing {dir:?}");
			// Iterator over all the files in the directory.
			let mut dir_contents = tokio::fs::read_dir(dir).await?;

			// This stores all of the recursive calls to `process_file`.
			// These are started up in new threads and executed asynchronously,
			// then joined together at the end.
			// Doing this asynchronously results in a massive speedup.
			let mut file_handles = Vec::new();

			// Any descendent `EPkg`s that should be merged into this one.
			let mut children: Vec<EPkg> = Vec::new();



			// Both arms of this branch are responsible for iterating over the directory
			// and filling up `children`.
			// Things are split up in order to optimize the code, since one branch skips over
			// files that match the examples found in `epkg.core()`, and most files don't have examples.
			match epkg.core().examples.as_ref() {
				None => {
					while let Some(entry) = dir_contents.next_entry().await? {
						// directories are handled in recursive calls
						if !entry.file_type().await?.is_file() { continue; }
						// recursively process any files in this directory
						// if let Ok(epkg) = process_file(entry.path()).await {
						// 	children.push(epkg);
						// }
						file_handles.push(tokio::spawn(process_file(entry.path())));
					}
				},
				// `epkg` has examples. We shouldn't parse any files that are intended to be examples.
				Some(examples) => {
					let to_ignore: HashSet<Box<str>> = examples.iter()
						.map(|e| e.path.clone())
						.collect();
					while let Some(entry) = dir_contents.next_entry().await? {
				
						// directories are handled in recursive calls
						if !entry.file_type().await?.is_file() { continue; }
		
						// don't even try to parse the examples
						let path: PathBuf = entry.path().with_extension("");
						let filename = path.file_name().unwrap().to_str().unwrap();
						if to_ignore.contains(filename) {
							continue;
						}
						// recursively process any files in this directory
						// if let Ok(epkg) = process_file(entry.path()).await {
						// 	children.push(epkg);
						// }
						file_handles.push(tokio::spawn(process_file(entry.path())));
					}
				}
			};

			for file_handle in file_handles {
				if let Ok(epkg) = file_handle.await? {
					children.push(epkg);
				}
			}

			// Add the processed children to the parent file, but only if there are any children to add.
			// We are doing this `len` check in order to avoid triggering the `error`s in the
			// bottom branches of the `match &mut epkg` statement.
			if children.len() > 0 {
				match &mut epkg {
					EPkg::Class(cls_pkg) => {
						for child in children {
							match child {
								EPkg::Function(pkg) => cls_pkg.functions.push(pkg),
								EPkg::Method(pkg) => cls_pkg.methods.push(pkg),
								EPkg::Operator(pkg) => cls_pkg.ops.push(pkg),
								EPkg::Value(pkg) => cls_pkg.values.push(pkg),
								
								p => todo!("is this even possible? child = {:?}", p.core().name),
							}
						
						}
					},
					EPkg::Lib(lib_pkg) => for child in children {
						match child {
							EPkg::Function(pkg) => lib_pkg.functions.push(pkg),
							// EPkg::Method(pkg) => lib_pkg.methods.push(pkg),
							EPkg::Lib(pkg) => lib_pkg.sublibs.push(pkg),
							EPkg::Value(pkg) => lib_pkg.values.push(pkg),
							
							p => todo!("is this even possible? child = {:?}", p.core().name),
						}
					},
	
					EPkg::Function(par) => todo!("is this even possible? par = Function({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Method(par) => todo!("is this even possible? par = Method({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Value(par) => todo!("is this even possible? par = Value({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Event(par) => todo!("is this even possible? par = Event({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
					EPkg::Operator(par) => todo!("is this even possible? par = Operator({:?}). children.len() = {}", par.core.name.as_ref(), children.len()),
				}
			}
		}
	

		Ok(epkg)
	})
}



/// Scans through `dir` and parses all packages defined with it. The `ignore` parameter specifies which
/// filepaths should be skipped when iterating. It is obtained from parsing the `examples` parameter of the parent component.
// fn process_dir(dir: PathBuf, ignore: HashSet<Box<str>>) -> Pin<Box<dyn Send + Sync + Future<Output = Result<Vec<EPkg>, Error>>>> {
// 	Box::pin(async move {
		
// 	})
// }
pub struct DocPackages {
	pub types: Vec<ClassPkg>,
	pub globals: Vec<EPkg>,
	pub events: Vec<EventPkg>,
}

const NAMED_TYPES_PATH: &'static str = "docs/namedTypes";
const GLOBALS_PATH: &'static str = "docs/global";
const EVENTS_PATH: &'static str = "docs/events/standard";

impl DocPackages {


	pub async fn new() -> Result<DocPackages, Error> {

		// stores all the class packages from `namedTypes`
		let tys_h = tokio::spawn(async {
			info!("Loading {NAMED_TYPES_PATH:?}...");
			let mut stream = tokio::fs::read_dir(NAMED_TYPES_PATH).await?;
			let mut classes = Vec::new();

			let mut file_handles = Vec::new();
			
			while let Some(entry) = stream.next_entry().await? {
				let ft = entry.file_type().await?;
				if !ft.is_file() { continue; } 
				file_handles.push(tokio::spawn(process_file(entry.path())));
			}
			for file_handle in file_handles {
				match file_handle.await? {
					Ok(EPkg::Class(cls)) => classes.push(cls),
					Ok(pkg) => unreachable!("Found a type that isn't a class. What would the Java creators think of this.... {pkg:?}"),
					Err(e) => return Err(e)
				}
			}
			// println!("read {num_read} files");
			return Ok(classes);
		});
		
		// stores all the globals from `globals`
		let globs_h: JoinHandle<Result<Vec<EPkg>, Error>> = tokio::spawn(async {
			info!("Loading {GLOBALS_PATH:?}...");
			let mut stream = tokio::fs::read_dir(GLOBALS_PATH).await?;
			let mut pkgs = Vec::new();

			let mut file_handles = Vec::new();
			
			while let Some(entry) = stream.next_entry().await? {
				let ft = entry.file_type().await?;
				if !ft.is_file() { continue; }

				file_handles.push(tokio::spawn(process_file(entry.path())));
			}
			for file_handle in file_handles {
				pkgs.push(file_handle.await??);
			}
			// println!("read {num_read} files");
			return Ok(pkgs);
		});

		// stores all the events from `events/standard`
		let evs_h = tokio::spawn(async {
			info!("Loading {EVENTS_PATH:?}...");
			let mut stream = tokio::fs::read_dir(EVENTS_PATH).await?;
			let mut events = Vec::new();

			let mut file_handles = Vec::new();
			
			while let Some(entry) = stream.next_entry().await? {
				let ft = entry.file_type().await?;
				if !ft.is_file() { continue; } 
				file_handles.push(tokio::spawn(process_file(entry.path())));
			}
			for file_handle in file_handles {
				match file_handle.await? {
					Ok(EPkg::Event(event)) => events.push(event),
					Ok(pkg) => unreachable!("Found an event that isn't an event. What....? {pkg:?}"),
					Err(e) => return Err(e)
				}
			}
			// println!("read {num_read} files");
			return Ok(events);
		});

		match (tys_h.await?, globs_h.await?, evs_h.await?) {
			(Ok(types), Ok(globals), Ok(events)) => {
				Ok(Self{types, globals, events})
			},
			e => panic!("{e:?}")
		}
	}

	pub fn total_len(&self) -> usize {
		let mut total = self.types.len() + self.globals.len() + self.events.len();
		for cls in &self.types {
			total += cls.functions.len() + cls.methods.len() + cls.ops.len() + cls.values.len();
		}
		for lib in &self.globals {
			if let  EPkg::Lib(lib) = lib  {
				total += lib.functions.len() + lib.values.len() + lib.sublibs.len();
			} else {
				// info!("nonlib global: {lib:?}");
			}
		}
		info!("num events: {}", self.events.len());

		total
	}
}



#[inline(always)]
async fn gather_files() -> Result<(), Error> {

	let pkgs = DocPackages::new().await?;

	let mut cls_map = HashMap::with_capacity(pkgs.types.len());
	for cls in &pkgs.types {
		cls_map.insert(cls.core.name.as_ref(), cls);
	}
	// for 


	Ok(())
}



#[tokio::main]
async fn main() -> Result<(), Error> {
	// use log;
	env_logger::init();


	use std::time::Instant;

    let now = Instant::now();

    // Code block to measure.
	let pkgs = DocPackages::new().await?;



	let elapsed = now.elapsed();
	let total = pkgs.total_len();
	// let total = pkgs.events.len() + pkgs.types.len() + pkgs.globals.len();
	println!("Processed {total} files in {elapsed:.2?}.");


	Ok(())

}
