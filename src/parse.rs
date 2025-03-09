
use std::path::PathBuf;
use std::{ops::FromResidual, path::Path};
use std::ops::{Residual, Try};
use log::{debug, log_enabled, trace};

use crate::result::DocFormatError;
use crate::result::{ParseErr, Error};
use crate::lex::{Kw, Lit, Punc, Token, TokenIter};
use crate::package::{ClassPkg, EPkg, EventDatum, EventLink, EventPkg, Example, FnArg, FunctionPkg, LibPkg, MethodPkg, OperatorPkg, Overload, PkgCore, ValuePkg};
use derive_more::Debug;



/// A key to a Lua table corresponding to a documentation file.
/// This is either a string or a special recognized keyword.
/// We don't allow booleans to be used to index tables because the documentation currently does not utilize that.
#[derive(Debug)]
pub enum Key {
	/// Custom strings that do not correspond to keywords. These typically appear in the `examples` sections.
	Str(Box<str>),
	Num(f64),
	/// Recognized keywords.
	Kw(Kw),
}

impl TryFrom<Key> for Box<str> {
	type Error = DocFormatError;

	fn try_from(input: Key) -> Result<Self, Self::Error> {
		match input {
			Key::Kw(kw) => Ok(kw.boxed_str()),
			Key::Str(s) => Ok(s),
			key@Key::Num(_) => Err(DocFormatError::UnexpectedKey { key, msg: "Expected a string or keyword."})

		}
	}
}

impl Key {
	pub fn unwrap_str(self) -> Box<str> {
		match self {
			Self::Str(str) => str,
			t => panic!("Expected string. Got {t:?}")
		}
	}

	pub fn unwrap_num(self) -> f64 {
		match self {
			Self::Num(f) => f,
			t => panic!("Expected number. Got {t:?}")
		}
	}

	pub fn unwrap_key(self) -> Kw {
		match self {
			Self::Kw(kw) => kw,
			t => panic!("Expected keyword. Got {t:?}")
		}
	}
	pub fn str_or<E>(self, f: impl Fn(Key) -> E) -> Result<Box<str>, E> {
		match self {
			Self::Str(inner) => Ok(inner),
			key => Err(f(key))
		}
	}
	
	pub fn kw_or<E>(self, f: impl Fn(Key) -> E) -> Result<Kw, E> {
		match self {
			Self::Kw(inner) => Ok(inner),
			key => Err(f(key))
		}
	}

	pub fn str_or_msg(self, msg: &'static str) -> Result<Box<str>, DocFormatError> {
		match self {
			Self::Str(inner) => Ok(inner),
			key => Err(DocFormatError::UnexpectedKey { key, msg })
		}
	}

	/// Wants a keyword or a string.
	pub fn try_into_str(self, msg: &'static str) -> Result<Box<str>, DocFormatError> {
		match self {
			Self::Str(inner) => Ok(inner),
			Self::Kw(inner) => Ok(inner.boxed_str()),
			key => Err(DocFormatError::UnexpectedKey { key, msg })
		}
	}

	pub fn num_or_msg(self, msg: &'static str) -> Result<f64, DocFormatError> {
		match self {
			Self::Num(inner) => Ok(inner),
			key => Err(DocFormatError::UnexpectedKey { key, msg })
		}
	}


	pub fn kw_or_msg(self, msg: &'static str) -> Result<Kw, DocFormatError> {
		match self {
			Self::Kw(inner) => Ok(inner),
			key => Err(DocFormatError::UnexpectedKey { key, msg })
		}
	}
}



pub type Tbl = Vec<(Key, Val)>;
pub type Arr = Vec<Val>;

/// A value in a table. Note that this supports all literals except `Nil`, which is ignored.
#[derive(Debug)]
pub enum Val {
	Str(Box<str>),
	Num(f64),
	Bool(bool),
	
	Arr(Arr),
	Tbl(Tbl)
}

impl Val {

	pub fn str_or_msg(self, msg:&'static str) -> Result<Box<str>, DocFormatError> {
		match self {
			Self::Str(inner) => Ok(inner),
			val => Err(DocFormatError::UnexpectedValue { val, msg })
		}
	}
	pub fn num_or_msg(self, msg:&'static str) -> Result<f64, DocFormatError> {
		match self {
			Self::Num(inner) => Ok(inner),
			val => Err(DocFormatError::UnexpectedValue { val, msg })
		}
	}
	pub fn bool_or_msg(self, msg:&'static str) -> Result<bool, DocFormatError> {
		match self {
			Self::Bool(inner) => Ok(inner),
			val => Err(DocFormatError::UnexpectedValue { val, msg })
		}
	}

	pub fn arr_or_msg(self, msg: &'static str) -> Result<Arr, DocFormatError> {
		match self {
			Self::Arr(inner) => Ok(inner),
			val => Err(DocFormatError::UnexpectedValue { val, msg })
		}
	}

	pub fn tbl_or_msg(self, msg: &'static str) -> Result<Tbl, DocFormatError> {
		match self {
			Self::Tbl(inner) => Ok(inner),
			val => Err(DocFormatError::UnexpectedValue { val, msg })
		}
	}

	
}





macro_rules! expect_punc {
	($iter:ident; $($punc:path),*) => {
		$(
			trace!("expecting punc {:?}", $punc);
			match $iter.next() {
				None => do yeet ParseErr::EOI,
				Some(Token::Punc($punc)) => (),
				Some(token) => do yeet ParseErr::PuncExpected(token, $punc)
			}
		)*
 	};
}


pub trait FromTokens: Sized {
	fn from_tokens(iter: &mut TokenIter) -> Result<Self, ParseErr>;
}

impl FromTokens for Key {
	fn from_tokens(iter: &mut TokenIter) -> Result<Self, ParseErr> {

		match  iter.next().ok_or(ParseErr::EOI)? {
			Token::Ident(str) => Ok(Self::Str(str)),
			Token::Kw(kw) => Ok(Self::Kw(kw)),
			// Literals must come between square brackets
			Token::Punc(Punc::LBrace) => {

				let lit = iter.next().ok_or(ParseErr::EOI)?.lit_or(ParseErr::KeyExpected)?;
				expect_punc!(iter; Punc::RBrace);
				match lit {
					Lit::Str(str) => Ok(Self::Str(str)),
					Lit::Num(num) => Ok(Self::Num(num)),
					lit => Err(ParseErr::UnsupportedKeyType(lit)),
				}
			},
			t => Err(ParseErr::KeyExpected(t)),
		}
	}
}

/// Internal function used when parsing values.
/// Parses something as a table of keyvalue pairs
/// This function assumes that the opening bracket of a table was already consumed.
/// This function will also consume the closing bracket of a table.
fn parse_tbl(iter: &mut TokenIter) -> Result<Vec<(Key, Val)>, ParseErr> {
	let mut pairs = Vec::new();

	if let Token::Punc(Punc::RBracket) = iter.peek().ok_or(ParseErr::EOI)? {
		iter.next();
		return Ok(pairs);
	}

	loop {
		let key = Key::from_tokens(iter)?;
		
		expect_punc!(iter; Punc::Eq);

		if let Some(val) = Option::<Val>::from_tokens(iter)? {
			pairs.push((key, val));

		}

		match iter.next().ok_or(ParseErr::EOI)? {
			Token::Punc(Punc::RBracket) => break,
			Token::Punc(Punc::Comma) => {
				if let Token::Punc(Punc::RBracket) = iter.peek().ok_or(ParseErr::EOI)? {
					iter.next();
					break
				}
			},
			t => return Err(ParseErr::PuncExpected(t, Punc::Comma))
		}



		// if let Token::Punc(Punc::RBracket) = iter.peek()?
	}
	Ok(pairs)
}

/// Internal function used when parsing values.
/// Parses something as an array of values.
/// This function assumes that the opening bracket of an array was already consumed.
/// This function will also consume the closing bracket of an array.
fn parse_arr(iter: &mut TokenIter) -> Result<Vec<Val>, ParseErr> {
	let mut vals = Vec::new();

	if let Token::Punc(Punc::RBracket) = iter.peek().ok_or(ParseErr::EOI)? {
		iter.next();
		return Ok(vals);
	}

	loop {
		match Option::<Val>::from_tokens(iter)? {
			Some(val) => vals.push(val),
			None => ()
		}

		match iter.next().ok_or(ParseErr::EOI)? {
			Token::Punc(Punc::RBracket) => break,
			Token::Punc(Punc::Comma) => {
				if let Token::Punc(Punc::RBracket) = iter.peek().ok_or(ParseErr::EOI)? {
					iter.next();
					break
				}
			},
			t => return Err(ParseErr::PuncExpected(t, Punc::Comma))
		}
	}
	Ok(vals)
}


impl FromTokens for Option<Val> {
	fn from_tokens(iter: &mut TokenIter) -> Result<Option<Val>, ParseErr> {
		match iter.next().ok_or(ParseErr::EOI)? {
			Token::Lit(lit) => {
				match lit {
					Lit::Str(v) => Ok(Some(Val::Str(v))),
					Lit::Num(v) => Ok(Some(Val::Num(v))),
					Lit::Bool(v) => Ok(Some(Val::Bool(v))),
					Lit::Nil => Ok(None),
				}
				// Ok(Self::Lit(lit))
			},
			
			Token::Punc(Punc::LBracket) => {
				// Check for an equals sign. This determines whether we are parsing a table or array.
				if matches!(iter.peek(), Some(Token::Punc(Punc::LBrace)))
				|| matches!(iter.peek2(), Some(Token::Punc(Punc::Eq)))
				{
					parse_tbl(iter).map(|v| Some(Val::Tbl(v)))
				} else {
					parse_arr(iter).map(|v| Some(Val::Arr(v)))
				}
				// match iter.peek2().ok_or(ParseErr::EOI)? {
				// 	Token::Punc(Punc::Eq) => 
				// 	_ => ,
				// }
			},

			t => Err(ParseErr::ValExpected(t))


		}
	}
}

/// Stores the path to a lua file as well as its parsed contents.
#[derive(Debug)]
pub struct LuaFile {
	/// The path that this lua file originated from.
	/// For our purposes it is useful to have this be remembered by the struct.
	pub path: Box<Path>,
	pub contents: Tbl,
}


impl LuaFile {
	#[inline(always)]
	fn from_path_contents(path: Box<Path>, contents: &str) -> Result<Self, Error> {
		let mut iter = TokenIter::from_input(contents)?;
		iter.expect_punc(Punc::Return)?;

		match Option::<Val>::from_tokens(&mut iter)? {
			Some(Val::Tbl(tbl)) => Ok(Self{path, contents: tbl}),
			v => panic!("file was not a table! instead it was {v:?}"),
		}

	}
	pub async fn from_path(path: Box<Path>) -> Result<Self, Error> {
		let contents = tokio::fs::read_to_string(&path).await?;
		Self::from_path_contents(path, &contents)
	}
}






// =============================================================================
// PARSE TO OUR STRUCTS
// =============================================================================

impl Example {
	pub fn from_tbl_entry(key: Key, val: Val) -> Result<Self, DocFormatError> {
		let path = key.try_into_str("path must be a string!")?;
		let pairs = val.tbl_or_msg("Examples must be given as a table!")?;

		let mut title = None;
		let mut description = None;
		for (key, val) in pairs {

			let kw = key.kw_or_msg("Expected `title` or `description`.")?;

			let str = val.str_or_msg("[Example] string expected!")?;

			match kw {
				Kw::Title => title = Some(str),
				Kw::Description => description = Some(str),
				_ => kw.unsuported("Example")?

			}
		}
		Ok(Self{ path, title, description })
	}
}


fn parse_arg(pairs: Tbl) -> Result<FnArg, DocFormatError> {
	let mut name = None;
	let mut description = None;
	let mut default = None;
	let mut ty = None;
	let mut optional = false;
	let mut table_params = None;
	for (key, val) in pairs {

		let kw = key.kw_or_msg("Invalid specifier for function arguments/returns. Expected `name`, `description`, `default`, `type`, `optional`, or `tableParams`.")?;

		match kw {
			Kw::Name => name = Some(val.str_or_msg("`name` must be a string!")?),
			Kw::Description => description = Some(val.str_or_msg("`description` must be a string!")?),
			Kw::Type => ty = Some(val.str_or_msg("`type` must be a string!")?),
			
			Kw::Default => {
				match val {
					Val::Str(v) => default = Some(Lit::Str(v)),
					Val::Num(v) => default = Some(Lit::Num(v)),
					Val::Bool(v) => default = Some(Lit::Bool(v)),
					Val::Arr(_) => return Err(DocFormatError::UnexpectedValue { val, msg: "Default value must be a primitive type!" }),
					Val::Tbl(_) => return Err(DocFormatError::UnexpectedValue { val, msg: "Default value must be a primitive type!" }),
				}
			},
			Kw::Optional => optional = val.bool_or_msg("`optional` must be a boolean!`")?,
			Kw::TableParams => table_params = Some(process_fn_args(val)?),
			
			kw => kw.unsuported("arguments/returns")?
		}
	}
	Ok(FnArg{ name, description, default, ty, optional, table_params })
}

fn process_fn_args(val: Val) -> Result<Vec<FnArg>, DocFormatError> {
	match val {
		
		Val::Arr(arr) => {
			let mut fns = Vec::with_capacity(arr.len());
			for v in arr {
				fns.push(parse_arg(v.tbl_or_msg("must be table!")?)?);
			}
			Ok(fns)
		},
		Val::Tbl(defn) => Ok(vec![parse_arg(defn)?]),
		Val::Str(name) => Ok(vec![FnArg{name: Some(name), ..Default::default()}]),
		_ => todo!("unexpected val: {val:?}"),
	}
}


fn parse_overload(pairs: Tbl) -> Result<Overload, DocFormatError> {
	let mut description = None;
	let mut right_ty = None;
	let mut result_ty = None;
	for (key, val) in pairs {

		let kw = key.kw_or_msg("Invalid specifier for function arguments/returns. Expected `description`, `result_ty`, or `right_ty`.")?;

		match kw {
			Kw::RightType => right_ty = Some(val.str_or_msg("`right_ty` must be a string!")?),
			Kw::Description => description = Some(val.str_or_msg("`description` must be a string!")?),
			Kw::ResultType => result_ty = Some(val.str_or_msg("`right_ty` must be a string!")?),
			kw => kw.unsuported("overload")?
		}
	}
	Ok(Overload{description, right_ty, result_ty})
}

fn process_overloads(val: Val) -> Result<Vec<Overload>, DocFormatError> {
	match val {
		Val::Str(_) => todo!(),
		Val::Num(_) => todo!(),
		Val::Bool(_) => todo!(),
		Val::Arr(arr) => {
			let mut overloads = Vec::with_capacity(arr.len());
			for v in arr {
				overloads.push(parse_overload(v.tbl_or_msg("overload must be a table!")?)?);
			}
			Ok(overloads)
		},
		Val::Tbl(pairs) => Ok(vec![parse_overload(pairs)?]),
	}
}

/// Parses stuff like
/// ```lua
///	["activator"] = {
///		type = "tes3reference",
///		readOnly = true,
///		description = "The actor attempting to trigger the event.",
///	},
/// ```
fn parse_event_datum(key: Key, val: Val) -> Result<EventDatum, DocFormatError> {
	// println!("got key: {key:?}");
	let tbl = val.tbl_or_msg("must be table!")?;
	// key.unwrap_str();
	// panic!();
	// let name = key.str_or_msg("event data name must be string!")?;
	let name = match key {
		Key::Str(s) => s,
		Key::Kw(kw) =>kw.boxed_str(),
		Key::Num(_) => todo!(),
	};
	// let name = key.unwrap_str();
	let mut description = None;
	let mut ty = None;

	let mut read_only = false;
	let mut optional = false;

	let mut default: Option<_> = None;

	for (key, val) in tbl {
		// println!("\tmatching ({key:?}, {val:?})");
		match key.kw_or_msg("!!")? {
			Kw::Description => description = Some(val.str_or_msg("!!")?),
			Kw::Type => ty = Some(val.str_or_msg("!!")?),

			Kw::ReadOnly => read_only = val.bool_or_msg("!!")?,
			Kw::Optional => optional = val.bool_or_msg("!!")?,

			Kw::Default => default = match val {
				Val::Num(n) => Some(Lit::Num(n)),
				Val::Str(n) => Some(Lit::Str(n)),
				Val::Bool(n) => Some(Lit::Bool(n)),
				_ => panic!("bad val type for default eventdata: {val:?}"),
			},
			kw => panic!("bad keyword in event data: {kw:?}")
		}
	}
	// println!("\treturning");

	Ok(EventDatum{name, ty, read_only, optional, default, description})
}


fn process_event_data(val: Val) -> Result<Vec<EventDatum>, DocFormatError> {
	let tbl = val.tbl_or_msg("blah")?;
	let mut data = Vec::with_capacity(tbl.len());
	
	for (k, v) in tbl {
		data.push(parse_event_datum(k, v)?);
	}
	Ok(data)
}

macro_rules! set_bool {
	($id:ident <- $val:ident) => {
		$id = $val.bool_or_msg(concat!(stringify!($id), " must be a bool!"))?;
	};
}
macro_rules! set_mstr {
	($id:ident <- $val:ident) => {
		$id = Some($val.str_or_msg(concat!(stringify!($id), " must be a string!"))?);
	};
}




impl EPkg {

fn from_lua_file(file: LuaFile) -> Result<Self, DocFormatError> {

	let contents = file.contents;

	// Core values
	

	let mut description = None;
	let mut experimental: bool = false;
	let mut deprecated: bool = false;
	let mut examples:  Option<Vec<Example>> = None;

	let mut file_type_str: Option<Box<str>> = None;


	
	// value specific
	let mut valuetype = None;
	let mut default: Option<Lit> = None;


	// function / method specific 
	let mut args = Vec::new();
	let mut rets = Vec::new();


	// class specific
	let mut inherits = None;
	let mut is_abstract = false;

	// value specific
	let mut read_only = false;

	// libs specific
	let mut link = None;

	// event specific
	let mut filter = None;
	let mut blockable = false;
	let mut event_data = Vec::new();
	let mut related: Vec<Box<str>> = Vec::new();
	let mut links = Vec::new();

	// let mut info = FileInfo{file_type: FileType::Class, description: None};

	// operator specific
	let mut overloads = Vec::new();
	for (key, val) in contents {
		let kw = key.kw_or_msg("only keywords are allowed at the package root.")?;
		match kw {
			Kw::Arguments => args = process_fn_args(val)?,
			Kw::Returns => rets = process_fn_args(val)?,
			Kw::Overloads => overloads = process_overloads(val)?,
			
			Kw::Examples => {
				let arr = val.tbl_or_msg("Examples must be specified as a table!")?;
				examples = Some(arr
					.into_iter()
					.map(|(k, v)| Example::from_tbl_entry(k, v))
					.try_collect()?
				);
				// examples = Some(Vec::<Example>::from_tokens(iter)?);
			},
			Kw::EventData => {
				let tbl = val.tbl_or_msg("aa!!")?;
				event_data = Vec::with_capacity(tbl.len());
				// panic!("processing event data: {:#?}", &tbl[..10]);
				for (k, v) in tbl {
					event_data.push(parse_event_datum(k, v)?);
				}
			},

			Kw::Type => set_mstr!(file_type_str <- val),
			Kw::Description => set_mstr!(description <- val),
			Kw::ValueType => set_mstr!(valuetype <- val),
			Kw::Inherits => set_mstr!(inherits <- val),
			Kw::Link => set_mstr!(link <- val),
			Kw::IsAbstract => set_bool!(is_abstract <- val),
			Kw::ReadOnly =>set_bool!(read_only <- val),
			Kw::Deprecated => set_bool!(deprecated <- val),
			Kw::Experimental =>set_bool!(experimental <- val),
			Kw::Blockable => set_bool!(blockable <- val),
			Kw::Filter => set_mstr!(filter <- val),


			Kw::Links => {
				for (k, v) in val.tbl_or_msg("msg")? {
					links.push(EventLink{
						name: k.str_or_msg("link name must be a string")?,
						path: v.str_or_msg("link `path` must be a string!")?
					});
				}
			},

			

			Kw::Default => {
				match val {
					Val::Str(v) => default = Some(Lit::Str(v)),
					Val::Num(v) => default = Some(Lit::Num(v)),
					Val::Bool(v) => default = Some(Lit::Bool(v)),
					Val::Arr(_) => return Err(DocFormatError::UnexpectedValue { val, msg: "Default value must be a primitive type!" }),
					Val::Tbl(_) => return Err(DocFormatError::UnexpectedValue { val, msg: "Default value must be a primitive type!" }),
				}
			},
			Kw::Related => {
				related = val
					.arr_or_msg("Related events must be specified as an array")?
					.into_iter()
					.map(|v| v.str_or_msg("Related event must be a string!"))
					.try_collect()?;
			},
			kw => todo!("support {kw:?}"),
		}
	}

	let core = PkgCore {
		description, examples, experimental, deprecated,
		name: {
			let fname = file.path.file_name().unwrap().to_str().unwrap();
			debug_assert!(fname.ends_with(".lua"));
			Box::from(&fname[..fname.len() - 4])
		},
		path: file.path
	};

	let file_type_str = file_type_str.expect("Error: no valuetype specified!");

	// Handle `value` here to avoid a clone later
	if file_type_str.as_ref() == "value" {
		return Ok(EPkg::Value(ValuePkg{core, ty: valuetype, default, read_only}));
	}

	if valuetype.is_some() {
		if rets.len() == 0 {
			rets = vec![FnArg{ty: valuetype, ..Default::default()}];
		} else {
			debug_assert!(rets.len() == 1, "Error: Cannot specify both valuetype and returns");
			debug_assert!(rets[0].ty.is_none(), "Error: Cannot specify both valuetype and returns");
			rets[0].ty = valuetype;
		}
	}
	debug!("returning packagetype {file_type_str}");
	// TODO: trigger error if value does not match parsed parameters
	match file_type_str.as_ref() {
		"class" => Ok(EPkg::Class(ClassPkg::new(core, is_abstract, inherits))),
		"function" => Ok(EPkg::Function(FunctionPkg{core, args, rets})),
		"method" => Ok(EPkg::Method(MethodPkg{core, args, rets})),
		"lib" => Ok(EPkg::Lib(LibPkg::new(core, link))),
		"event" => Ok(EPkg::Event(EventPkg{core, filter, blockable, event_data, links, related})),
		"operator" => Ok(EPkg::Operator(OperatorPkg{core, overloads})),
		_ => do yeet DocFormatError::BadPkgTy(file_type_str)
		// _ => do yeet ParseErr{token}
	}
}


pub async fn parse_from_file(path: &Path) -> Result<Self, Error> {
	let file = LuaFile::from_path(Box::from(path)).await?;
	Ok(Self::from_lua_file(file)?)
}
}



#[cfg(test)]
mod tests {

	use super::*;

	#[test]
	fn parse_table() -> Result<(), Error> {
		// let src = r#"{ key = "something" }"#;

		let path = Path::new("docs/events/standard/activate.lua");
		dbg!(&path);
		let rt = tokio::runtime::Runtime::new().unwrap();
		let epkg = rt.block_on(EPkg::parse_from_file(path))?;
		// println!("parsed: {epkg:#?}");


		Ok(())

	}
}