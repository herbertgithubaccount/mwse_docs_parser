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
type BoxErr = Box<dyn std::error::Error + Send + Sync>;

mod parse;
mod writer;
pub mod package;
pub mod error;
use std::{path::{Path, PathBuf}, pin::Pin};

use derive_more::{Display, From};
use error::Error;
use lex::LexError;
use log::{debug, error, info, trace, warn};
use package::{ClassPkg, EPkg, EventPkg, FunctionPkg, LibPkg, MethodPkg, OperatorPkg, PkgCore, ValuePkg};
pub use parse::*;
use writer::Writable;


#[derive(Debug, From)]
enum MainError {
	Io(std::io::Error),
	Lex(LexError),
	Parse(ParseErr),
	Msg(String),
}

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
		// unsafe {
		// 	// let ptr = self as *mut _;
		// 	// ptr.write_bytes(val, count);
		// 	// ptr.write(val);
		// 	let vec = &mut self.classes;
		// 	if vec.capacity() == vec.len() {
		// 		vec.reserve(1);
		// 	}
		// 	std::mem::cop
		// 	vec.len

		// }
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
		// let mut filepaths = Vec::new();
		while let Some(entry) = stream.next_entry().await? {
			// dbg!(entry);
			// if entry.
			// info!("processing {entry:?}");
			let ft = entry.file_type().await?;
			if ft.is_file() {
				// filepaths.push(entry.path());
				file_handles.push(tokio::spawn(async move {
					let path = entry.path();
					// let epkg = EPkg::parse_from_file(&path).await;
					// (epkg, path)
					// (&path, EPkg::parse_from_file(&path).await)
					match EPkg::parse_from_file(&path).await {
						Ok(epkg) => return Some(epkg),
						// Ok(epkg) => pkgs.add_pkg(epkg),
						Err(Error::Lex(l)) => warn!("File {path:?} had could not be lexed: {l:?}"),
						Err(e) => error!("Could not parse file {path:?}: {e:?}")
					}
					None
				}));
				// pkgs.add_pkg(EPkg::parse_from_file(&entry.path()).await?);
			} else if ft.is_dir() {
				dir_handles.push(tokio::spawn(process(entry.path())));
				// pkgs.extend(process(entry.path()).await?);
			}
		}
		// for path in filepaths {
		// 	match EPkg::parse_from_file(&path).await {
		// 		Ok(epkg) => pkgs.add_pkg(epkg),
		// 		Err(Error::Lex(l)) => warn!("File {path:?} had could not be lexed: {l:?}"),
		// 		Err(e) => error!("Could not parse file {path:?}: {e:?}")
		// 	}
		// }
		for file_handle in file_handles {
			if let Some(epkg) = file_handle.await? {
				pkgs.add_pkg(epkg);
			}
			// match file_handle.await? {
			// 	// Ok(epkg) => Ok(epkg),
			// 	(Ok(epkg), _) => pkgs.add_pkg(epkg),
			// 	(Err(Error::Lex(l)), path)=> warn!("File {path:?} had could not be lexed: {l:?}"),
			// 	(Err(e), path) => error!("Could not parse file {path:?}: {e:?}")
			// }
			// match EPkg::parse_from_file(&path).await {
			// 	Ok(epkg) => pkgs.add_pkg(epkg),
			// 	Err(Error::Lex(l)) => warn!("File {path:?} had could not be lexed: {l:?}"),
			// 	Err(e) => error!("Could not parse file {path:?}: {e:?}")
			// }
		}

		for handle in dir_handles {
			pkgs.extend(handle.await??);
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
		
		let types;
		let globals;
		let events;
		// initialize the above variables using threads
		{

			let types_handle = tokio::spawn(process(PathBuf::from("docs/namedTypes")));
			let globals_handle = tokio::spawn(process(PathBuf::from("docs/global")));
			let events_handle = tokio::spawn(process(PathBuf::from("docs/events")));

			// named_types.
			match types_handle.await? {
				Ok(num) => types = num,
				Err(e) => panic!("{e:?}"),
			}
			match globals_handle.await? {
				Ok(num) => globals = num,
				Err(e) => panic!("{e:?}"),
			}
			
			match events_handle.await? {
				Ok(num) => events = num,
				Err(e) => panic!("{e:?}"),
			}
		}

		Ok(Self{ types, globals, events })
	}
}


async fn oldmain() -> Result<(), Error> {


	// dbg!(size_of::<EPkg>());
	// dbg!(size_of::<PkgCore>());
	// return Ok(());
	// env_logger::builder().filter_level(log::LevelFilter::Debug)
	// let input = r#"abc = { hi "this is a literal string}"#;
	// let input = include_str!("../docs/namedTypes/mwseMCMSlider/convertToLabelValue.lua");
	// let input = include_str!("../docs/namedTypes/mwseMCMSetting.lua");
	// let input = include_str!("../docs/namedTypes/mwseMCMSlider.lua");
	// let input = include_str!("../docs/global/tes3/equip.lua");
	// let input = include_str!("../docs/events/standard/activate.lua");

	// println!("lexing {input}");

	// let path = PathBuf::from("docs/namedTypes/tes3processManager/detectPresence.lua");
	// let contents = tokio::fs::read_to_string(&path).await?;
	// _= dbg!(process_str(contents, &path));

	// let path = PathBuf::from("docs/namedTypes/mgeCameraConfig.lua");
	// let mut read_dir = std::fs::read_dir("docs/namedTypes/mgeCameraConfig")?;
	// let path = PathBuf::from("docs/globals/math.lua");
	// let mut read_dir = std::fs::read_dir("docs/global/math")?;
	// let path = PathBuf::from("docs/namedTypes/mwseLogger.lua");
	// let mut read_dir = std::fs::read_dir("docs/namedTypes/mwseLogger")?;
	let path = PathBuf::from("docs/namedTypes/tes3mobilePlayer.lua");
	let mut read_dir = std::fs::read_dir("docs/namedTypes/tes3mobilePlayer")?;

	let epkg = EPkg::parse_from_file(&path).await?;
	debug!("got epkg: {epkg:?}");
	let EPkg::Class(mut cls_pkg) = epkg else {panic!()};


	let mut values = Vec::new();
	let mut functions = Vec::new();
	let mut methods = Vec::new();
	
	for path in read_dir {
		if path.is_err() {
			warn!("entry is error! entry: {:?}", path);
		}
		let entry = path?;
		debug!("processing entry: {:?}", entry.path());
		// let parent_name = Some(cls_pkg.core.name.clone());
		match EPkg::parse_from_file(&entry.path()).await {
			Ok(EPkg::Value(pkg)) => values.push(pkg),
			Ok(EPkg::Function(pkg)) => functions.push(pkg),
			Ok(EPkg::Method(pkg)) => methods.push(pkg),
			t => warn!("got something unexpected: {t:?}"),
			// _ => (),
		}
	}
	values.sort_by(|a, b| a.core.name.cmp(&b.core.name));
	functions.sort_by(|a, b| a.core.name.cmp(&b.core.name));
	methods.sort_by(|a, b| a.core.name.cmp(&b.core.name));

	cls_pkg.values = values;
	cls_pkg.functions = functions;
	cls_pkg.methods = methods;

	// info!("got {cls_pkg:?}");
	// dbg!(&cls_pkg);

	let mut writer = std::fs::File::create("output.md")?;
	cls_pkg.write_mkdocs(&mut writer, None)?;
	// return Ok(());


	Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Error> {
	// use log;
	env_logger::init();


	use std::time::Instant;
    let now = Instant::now();

	let pkg_tys;
	let pkg_globals;
	let pkg_events;
    // Code block to measure.
    {

		let named_types = tokio::spawn(process(PathBuf::from("docs/namedTypes")));
		let globals = tokio::spawn(process(PathBuf::from("docs/global")));
		let events = tokio::spawn(process(PathBuf::from("docs/events")));

		// named_types.
		match named_types.await? {
			Ok(num) => pkg_tys = num,
			Err(e) => panic!("{e:?}"),
		}
		match globals.await? {
			Ok(num) => pkg_globals = num,
			Err(e) => panic!("{e:?}"),
		}
		
		match events.await? {
			Ok(num) => pkg_events = num,
			Err(e) => panic!("{e:?}"),
		}
	}
	let elapsed = now.elapsed();
	let total = pkg_events.total_len() + pkg_tys.total_len() + pkg_globals.total_len();
	println!("Processed {total} files in {elapsed:.2?}.");
	// let tokens = lex::get_tokens(input);

	// if let Ok(tokens) = tokens {
	// 	println!("printing tokens!");
	// 	for (i, t) in tokens.iter().enumerate() {
	// 		println!("\t{i}) {t:?}")
	// 	}
	// 	let mut peekable = tokens.iter().peekable();
	// 	dbg!(Package::from_tokens(&mut peekable));

	// } else {
	// 	println!("Got tokens = {tokens:?}");
	// }


	Ok(())

}
