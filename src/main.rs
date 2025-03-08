#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![feature(yeet_expr)]
#![feature(try_trait_v2_yeet)]
#![feature(type_alias_impl_trait)]

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
use package::{ClassPkg, EPkg, LibPackage};
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

pub struct DirTokens {
}

fn process_str(contents: String, path: &Path) -> Result<bool, Error> {
	let mut iter = match lex::TokenIter::from_input(&contents) {
		Ok(t) => t,
		Err(e) => {
			let path = path;
			warn!("Could not lex {path:?}\n\tError: {e}");
			return Ok(false)
		}
	};
	debug!("Got tokens {:?}", iter.as_slice());
	// let mut iter = tokens.iter().peekable();
	match EPkg::from_tokens(&mut iter) {
		Err(ParseErr::BadPkgTy(ty)) => {
			warn!("Got a package with an invalid type!\n\t\
				path: {path:?}\n\t\
				type: {ty}");
				Ok(false)
		},
		Err(e) => {
			// println!("Error encountered when processing {path:?}:\n\t{e:?}");
			do yeet format!("Error encountered when processing {path:?}:\n\t{e:?}");
		},
		Ok(_) => {
			// println!("succesfully processed {entry:?}")
			Ok(true)
		}

		
	}
}

fn process(dir: PathBuf) -> Pin<Box<dyn Send + Sync + Future<Output = Result<usize, Error>>>> {
	Box::pin(async move {
	trace!("processing {dir:?}");
	let mut stream = tokio::fs::read_dir(dir).await?;
	let mut num_read = 0;

	while let Some(entry) = stream.next_entry().await? {
		// dbg!(entry);
		// if entry.
		info!("processing {entry:?}");
		let ft = entry.file_type().await?;
		if ft.is_file() {
			let path = entry.path();
			let contents = tokio::fs::read_to_string(&path).await?;
			match process_str(contents, &path) {
				Ok(true) => num_read += 1,
				Ok(false) => (),
				Err(e) => {
					// info!("parsed {num_read} files without error.... :(");
					do yeet e;
				}
			}
		} else if ft.is_dir() {
			match process(entry.path()).await {
				Ok(num) => num_read += num,
				Err(e) => {
					info!("parsed {num_read} files without error.... :(");
					do yeet e;
				}
			}
		}
	}
	// println!("read {num_read} files");
	return Ok(num_read);
})
}

#[tokio::main]
async fn main() -> Result<(), Error> {
	// use log;
	env_logger::init();
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
	let path = PathBuf::from("docs/namedTypes/mwseLogger.lua");
	let mut read_dir = std::fs::read_dir("docs/namedTypes/mwseLogger")?;

	let epkg = EPkg::parse_from_file(&path, None).await?;
	let EPkg::Class(mut cls_pkg) = epkg else {panic!()};


	let mut values = Vec::new();
	let mut functions = Vec::new();
	let mut methods = Vec::new();
	while let Some(entry) = read_dir.next() {
		let entry = entry?;
		let parent_name = Some(cls_pkg.core.name.clone());
		match EPkg::parse_from_file(&entry.path(), parent_name).await {
			Ok(EPkg::Value(pkg)) => values.push(pkg),
			Ok(EPkg::Function(pkg)) => functions.push(pkg),
			Ok(EPkg::Method(pkg)) => methods.push(pkg),

			_ => (),
		}
	}
	values.sort_by(|a, b| a.core.name.cmp(&b.core.name));
	functions.sort_by(|a, b| a.core.name.cmp(&b.core.name));
	methods.sort_by(|a, b| a.core.name.cmp(&b.core.name));

	cls_pkg.values = values;
	cls_pkg.functions = functions;
	cls_pkg.methods = methods;

	info!("got {cls_pkg:?}");

	let mut writer = std::fs::File::create("output.md")?;
	cls_pkg.write_mkdocs(&mut writer, None::<&ClassPkg>)?;
	return Ok(());



	use std::time::Instant;
    let now = Instant::now();

    // Code block to measure.
	let mut total = 0_usize;
    {

		let named_types = tokio::spawn(process(PathBuf::from("docs/namedTypes")));
		let globals = tokio::spawn(process(PathBuf::from("docs/global")));
		let events = tokio::spawn(process(PathBuf::from("docs/events")));

		// named_types.
		match named_types.await? {
			Ok(num) => total += num,
			Err(e) => panic!("{e:?}"),
		}
		match globals.await? {
			Ok(num) => total += num,
			Err(e) => panic!("{e:?}"),
		}
		
		match events.await? {
			Ok(num) => total += num,
			Err(e) => panic!("{e:?}"),
		}
	}
	let elapsed = now.elapsed();
	
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
