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
use std::{collections::HashMap, path::{Path, PathBuf}, pin::Pin};

use derive_more::{Display, From};
use error::Error;
use lex::LexError;
use log::{debug, error, info, trace, warn};
use package::{ClassPkg, EPkg, EventPkg, FunctionPkg, LibPkg, MethodPkg, OperatorPkg, PkgCore, ValuePkg};
pub use parse::*;
use writer::Writable;


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



fn process(dir: PathBuf) -> Pin<Box<dyn Send + Sync + Future<Output = Result<DirPackages, Error>>>> {
	Box::pin(async move {
		trace!("processing {dir:?}");
		let mut stream = tokio::fs::read_dir(dir).await?;
		let mut pkgs = DirPackages::default();
		let mut dir_handles = Vec::new();

		let mut file_handles = Vec::new();
		
		while let Some(entry) = stream.next_entry().await? {
			
			// Don't have to wait very long for this.
			// And waiting for it allows us to split the futures up based on return type.
			// This means less branching later on.
			let ft = entry.file_type().await?;

			if ft.is_dir() {
				dir_handles.push(tokio::spawn(process(entry.path())));
			} else if ft.is_file() {
				file_handles.push(tokio::spawn(async move {
					let path = entry.path();
					// Only return a file if we were able to parse it.
					// Log the errors instead of killing the whole program.
					match EPkg::parse_from_file(&path).await {
						Ok(epkg) => return Some(epkg),
						Err(Error::Lex(l)) => warn!("File {path:?} had could not be lexed: {l:?}"),
						Err(e) => error!("Could not parse file {path:?}: {e:?}")
					}
					None
				}));
			}
		}
		

		for handle in dir_handles {
			pkgs.extend(handle.await??);
		}
		for file_handle in file_handles {
			if let Some(epkg) = file_handle.await? {
				pkgs.add_pkg(epkg);
			}
		}
		// println!("read {num_read} files");
		return Ok(pkgs);
	})
}
pub struct DocPackages {
	pub types: DirPackages,
	pub globals: DirPackages,
	pub events: DirPackages,
}

impl DocPackages {
	pub async fn new() -> Result<DocPackages, Error> {
		

		// initialize the above variables using threads
		let tys_h = tokio::spawn(process(PathBuf::from("docs/namedTypes")));
		let globs_h = tokio::spawn(process(PathBuf::from("docs/global")));
		let evs_h = tokio::spawn(process(PathBuf::from("docs/events")));

		match (tys_h.await?, globs_h.await?, evs_h.await?) {
			(Ok(types), Ok(globals), Ok(events)) => {
				Ok(Self{types, globals, events})
			},
			e => panic!("{e:?}")
		}
	}
}



#[inline(always)]
async fn gather_files() -> Result<(), Error> {

	let pkgs = DocPackages::new().await?;

	let mut cls_map = HashMap::with_capacity(pkgs.types.classes.len());
	for cls in &pkgs.types.classes {
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
	let total = pkgs.events.total_len() + pkgs.types.total_len() + pkgs.globals.total_len();
	println!("Processed {total} files in {elapsed:.2?}.");


	Ok(())

}
