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
#![feature(iterator_try_collect)]

mod lex;

mod parse;
mod writer;
pub mod package;
pub mod result;
use std::{collections::{HashMap, HashSet}, path::PathBuf, pin::Pin, sync::Arc};

use result::Error;
use log::{debug, error, info, trace, warn};
use package::{ClassPkg, EPkg, EventPkg};
pub use parse::*;
use tokio::task::JoinHandle;
// use writer::Writable;


pub const NAMED_TYPES_PATH_IN: &'static str = "docs/namedTypes";
pub const GLOBALS_PATH_IN: &'static str = "docs/global";
pub const EVENTS_PATH_IN: &'static str = "docs/events/standard";
pub const NAMED_TYPES_PATH_OUT: &'static str = "output/types";
pub const GLOBALS_PATH_OUT: &'static str = "output/apis";
pub const EVENTS_PATH_OUT: &'static str = "output/events";




// struct DirPackage

// struct DirDocs {
// 	packages: Vec<Package>,
// }


/// Recursively processes a file and builds a `EPkg` from it.
/// If there is a directory with the same name as `path`, then that directory will be recursively processed
/// and any packages found there will be added to the package returned by this function.
/// The `Box::pin` stuff is necessary for recursive async functions.
fn process_file(path: PathBuf) -> Pin<Box<dyn Send + Sync + Future<Output = Result<EPkg, Error>>>> 
{ Box::pin(async move {
			
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
					if let Ok(epkg) = process_file(entry.path()).await {
						children.push(epkg);
					}
				}
			},
			// `epkg` has examples. We shouldn't parse any files that are intended to be examples.
			Some(examples) => {
				let to_ignore: HashSet<&str> = examples.iter()
					.map(|e| e.path.as_str())
					.collect();
				while let Some(entry) = dir_contents.next_entry().await? {
			
					// directories are handled in recursive calls
					if !entry.file_type().await?.is_file() { continue; }
	
					// Skip something if it corresponds to one of the examples.
					let path  = entry.path().with_extension("");
					let filename = path.file_name().unwrap().to_str().unwrap();
					if to_ignore.contains(filename) {
						continue;
					}
					// recursively process any files in this directory
					if let Ok(epkg) = process_file(entry.path()).await {
						children.push(epkg);
					}
				}
			}
		};

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
})}



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

impl DocPackages {


	pub async fn new() -> Result<DocPackages, Error> {

		// stores all the class packages from `namedTypes`
		let tys_h = tokio::spawn(async {
			info!("Loading {NAMED_TYPES_PATH_IN:?}...");
			let mut stream = tokio::fs::read_dir(NAMED_TYPES_PATH_IN).await?;
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

		// match tys_h.await? {
		// 	Err(e) => println!("error: {e:?}"),
		// 	_ => (),
		// }
		// panic!();
		
		// // stores all the globals from `globals`
		let globs_h: JoinHandle<Result<Vec<EPkg>, Error>> = tokio::spawn(async {
			info!("Loading {GLOBALS_PATH_IN:?}...");
			let mut stream = tokio::fs::read_dir(GLOBALS_PATH_IN).await?;
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
			info!("Loading {EVENTS_PATH_IN:?}...");
			let mut stream = tokio::fs::read_dir(EVENTS_PATH_IN).await?;
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
		// evs_h.await??;
		// panic!();

		match (tys_h.await?, globs_h.await?, evs_h.await?) {
			(Ok(types), Ok(globals), Ok(events)) => {
				Ok(Self{types, globals, events})
			},
			(Err(e), _, _) => panic!("Error processing types: {e:?}"),
			(_, Err(e), _) => panic!("Error processing globals: {e:?}"),
			(_, _, Err(e)) => panic!("Error processing events: {e:?}"),
			// e => panic!("error encountered!")
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
			}
		}

		total
	}
}


async fn read_and_write() -> Result<(), Error> {

	use std::time::Instant;

    let now = Instant::now();

    // Code block to measure.
	let pkgs = DocPackages::new().await?;

	let total = pkgs.total_len();
	{
		let mut class_map: HashMap<& str, & ClassPkg> = HashMap::with_capacity(pkgs.types.len());
		for cls in &pkgs.types {
			class_map.insert(cls.core.name.as_str(), cls);
		}
		// let class_map = class_map;
		let class_map = Arc::from(class_map);


		// let mut handles: Vec<JoinHandle<Result<(), Error>>> = Vec::with_capacity(pkgs.types.len());
		for cls in &pkgs.types {
			// let class_map = Arc::clone(&class_map);
			// handles.push(tokio::spawn(async move {
				let mut path = PathBuf::from(NAMED_TYPES_PATH_OUT);
				path.extend(cls.core.path.as_ref().components().skip(2));

				let path = path.with_extension("md");
				std::fs::create_dir_all(path.parent().unwrap())?;
				info!("writing to path: {path:?}");
				// for 
				let mut writer = std::fs::File::create(&path)?;
				// let mut writer = tokio::fs::File::create(&path).await?;

				cls.write_mkdocs(&mut writer, &class_map)?;
				
				// Ok(())
			// }));
		}
		// for handle in handles {
		// 	handle.await??;
		// }
		for epkg in &pkgs.globals {
			match epkg {
				EPkg::Lib(pkg) => {
					let mut path = PathBuf::from("output");
					path.push("globals");
					let parts = pkg.core.path.as_ref().components()
						.skip(2);
						// .collect();

					path.extend(parts);
					let path = path.with_extension("md");
					std::fs::create_dir_all(path.parent().unwrap())?;
					info!("writing to path: {path:?}");
					// for 
					let mut writer = std::fs::File::create(&path)?;
					// let mut writer = tokio::fs::File::create(&path).await?;

					pkg.write_mkdocs(&mut writer)?;
				},
				_ => (),
				// EPkg::Class(class_pkg) => todo!(),
				// EPkg::Function(function_pkg) => todo!(),
				// EPkg::Method(method_pkg) => todo!(),
				// EPkg::Value(value_pkg) => todo!(),
				// EPkg::Event(event_pkg) => todo!(),
				// EPkg::Operator(operator_pkg) => todo!(),
			}
		}
	}
	
	let elapsed = now.elapsed();
	// let total = pkgs.events.len() + pkgs.types.len() + pkgs.globals.len();
	println!("Processed {total} files in {elapsed:.2?}.");



	Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Error> {
	// use log;
	env_logger::init();

	read_and_write().await?;
	Ok(())

}
