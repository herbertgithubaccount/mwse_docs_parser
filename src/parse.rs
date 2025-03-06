use std::{borrow::Cow, default};

use crate::lex::{self, Lit, Punc, SpecialIdent, Token};


#[derive(Debug, Clone, Copy)]
pub struct Example<'a> {
	pub name: &'a str,
	pub description: &'a str,
}

#[derive(Debug, Clone, Copy)]
pub struct ClassPackage<'a> {
	/// The type from which this type inherits should be passed here. This will allow the documentation builders to build the proper inheritance chains. For example, when a function accepts tes3mobileActor, because tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer have inherits = "tes3mobileActor", the docs will be built with tes3mobileNPC, tes3mobileCreature, and tes3mobilePlayer parameters for that function automatically. This saves you the job of figuring out the complete inheritance chains.
	pub inherits: Option<&'a str>,
	///This is a flag for types that can't be accessed normally. There are some types which inherit from abstract ones.
	pub is_abstract: bool,
}


#[derive(Debug)]
pub struct ValuePackage<'a>{
	read_only: bool,
	value_type: &'a str,
	examples: Vec<Example<'a>>,

}


/// Stores an argument / return value of a function.
#[derive(Debug)]
pub struct FnArg<'a> {
	pub name: &'a str,
	pub ty: &'a str,
	pub optional: bool,
	pub description: &'a str,
	pub default: Option<&'a Lit<'a>>,
	pub table_params: Option<Vec<FnArg<'a>>>,
}

#[derive(Debug)]
pub struct FunctionPackage<'a> {
	pub args: Vec<FnArg<'a>>,
	pub rets: Vec<FnArg<'a>>,
}

#[derive(Debug)]
pub struct LibPackage<'a> {
	pub args: Vec<FnArg<'a>>,
	// pub rets: Vec<FnArg<'a>>,
}
#[derive(Debug)]
pub struct EventPackage<'a> {
	pub args: Vec<FnArg<'a>>,
	// pub rets: Vec<FnArg<'a>>,
}







pub enum PackageType<'a> {
	Class(ClassPackage<'a>),
	Function(FunctionPackage<'a>),
	Method(FunctionPackage<'a>),
	Value(ValuePackage<'a>),
	Lib(LibPackage<'a>),
	Event(EventPackage<'a>)
}

pub enum ValueType {

}



pub struct Package<'a> {
	/// Name of the package.
	pub name: &'a str,
	/// Description of the package.
	pub description: &'a str,
	/// The type of package, along with information specific to that package.
	pub ty: PackageType<'a>,
	/// Is this part of the experimental API? Default: `false`.
	pub experimental: bool,

	// pub key string The name of the file that generated this package.
	// pub type packageType The type definition for the package.
	// pub folder string The folder that the package was created from.
	// pub parent package The package this package is a child of.
	// pub namespace string The full namespace of the package.

	/// Allows marking definitions as deprecated. Those definitions aren't written to the web documentation.
	pub deprecated: bool,

	///A table containing the examples. Keys are the example's name/path to the example file.
	pub examples:  Vec<Example<'a>>,
	///For libraries with sub-namespaces such as mwse.mcm, etc., this array contians the nested namespaces.
	pub libs:  Vec<Package<'a>>,
}




use std::iter::Peekable;
// fn pa(iter: Peekabl)
// type TokenIter<'a> = std::iter::Peekable<impl Iterator<Item = &Token<'a>>>;
// Peekable;
#[derive(Debug, Clone)]
pub enum ParseErr<'a> {
	BadToken(&'a Token<'a>),
	/// The value for this special identifier was not provided.
	UnspecifiedKeyword(SpecialIdent),
	PuncExpected(Punc),
	/// A special identifier showed up in a place it shouldn't have.
	UnsupportedKeyword(SpecialIdent),
}

macro_rules! expect_punc {
	($iter:ident; $($punc:path),*) => {
		$(
			let Some(Token::Punc($punc)) = $iter.next() else { 
				do yeet ParseErr::PuncExpected($punc)
			};
		)*
 	};
}




/// Parses a function argument, including the opening brace;
fn parse_fn_arg<'a>(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<FnArg<'a>, ParseErr<'a>> {
	expect_punc!(iter; Punc::LBracket);

	let mut name: Option<&str> = None;
	let mut ty: Option<&str> = None;
	let mut optional = false;
	let mut description: Option<&str> = None;
	let mut default = None;
	let mut table_params = None;
	
	while let Some(token) = iter.next() {

		// The type of identifier to set.
		let sp = match token {
			Token::SpecialIdent(sp)  => *sp,
			// End early if we found the closing bracket
			Token::Punc(Punc::RBracket) => break,
			_ => do yeet ParseErr::BadToken(token),
		};

		expect_punc!(iter; Punc::Eq);

		// The literal to use when initializing the keyword
		let Some(Token::Lit(lit)) = iter.next() else {
			do yeet ParseErr::UnspecifiedKeyword(sp)
		};

		match sp {
			SpecialIdent::Name => {
				let Lit::Str(s) = lit else {panic!("bad literal!")};
				name = Some(s);
			},
			SpecialIdent::Description => {
				let Lit::Str(s) = lit else {panic!("bad literal!")};
				description = Some(s);
			},
			SpecialIdent::Optional => {
				let Lit::Bool(b) = lit else {panic!("bad literal!")};
				optional = *b;
			},
			SpecialIdent::Type => {
				let Lit::Str(s) = lit else {panic!("bad literal!")};
				ty = Some(s);
			},
			SpecialIdent::Default => {
				default = Some(lit);
			},
			SpecialIdent::TableParams => todo!("support for table parameters"),
			sp => do yeet ParseErr::UnsupportedKeyword(sp),
		}
		// consume the next character
		// This should be a comma or bracket.
		// Otherwise, we error.
		match iter.next() {
			Some(Token::Punc(Punc::Comma)) => continue,
			Some(Token::Punc(Punc::RBracket)) => break,
			Some(t) => do yeet ParseErr::BadToken(t),
			None => do yeet ParseErr::PuncExpected(Punc::RBracket)
		}

	}
	Ok(FnArg{
		name: name.expect("No name given!"),
		description: description.expect("No description given!"),
		ty: ty.expect("No type given!"),
		optional,
		default,
		table_params
	})

}


/// Parses the list of function arguments, including the opening brace.
fn parse_fn_args<'a>(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Vec<FnArg<'a>>, ParseErr<'a>> {
	
	expect_punc!(iter; Punc::Eq, Punc::LBracket);

	// If there's no left bracket, then there's only a single argument.
	if !matches!(iter.peek(), Some(Token::Punc(Punc::LBracket))) {
		return Ok(vec![parse_fn_arg(iter)?]);
	}
	// eat the bracket
	iter.next();

	let mut args = Vec::new();


	// While the next token is not a bracket
	while !matches!(iter.peek(), Some(Token::Punc(Punc::RBracket))) {
		args.push(parse_fn_arg(iter)?);
	}
	expect_punc!(iter; Punc::RBracket);

	Ok(args)
}

#[derive(Debug)]
pub struct FnRet<'a> {
	pub name: &'a str,
	pub ty: &'a str,
	pub optional: bool,
	pub description: &'a str,
	pub default: Option<lex::Lit<'a>>,
}


pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<Package<'a>, ParseErr<'a>> {

	// The value type string. Used to make sure we parsed the file correctly.
	let mut file_type_str: Option<&'a str> = None;



	let mut name: Option<&'a str> = None;
	let mut description: Option<&'a str> = None;
	let mut ty: Option<PackageType<'a>> = None;
	let mut experimental: Option<bool> = None;
	let mut deprecated: Option<bool> = None;
	let mut examples:  Option<Vec<Example<'a>>> = None;
	let mut libs:  Option<Vec<Package<'a>>> = None;

	// function / method specific 
	let mut fn_args = Vec::new();
	let mut fn_rets = Vec::new();


	// class specific
	let mut class_ty = String::new();

	// let mut info = FileInfo{file_type: FileType::Class, description: None};

	let mut iter = tokens.iter().peekable();

	while let Some(t) = iter.next() {
		match t {

			Token::SpecialIdent(sp) => {
				match *sp {
					SpecialIdent::Arguments => {
						fn_args = parse_fn_args(&mut iter)?;
					},
					SpecialIdent::Returns => {
						fn_rets = parse_fn_args(&mut iter)?;
					},
					SpecialIdent::ValueType => {
						expect_punc!(iter; Punc::Eq);
						if let Some(Token::Lit(Lit::Str(s))) = iter.next() {
							file_type_str = Some(s);
						} else {
							panic!("Invalid file type given!!");
						}
					}
					_ => todo!()
				}
			},
			
			Token::Lit(lit) => todo!(),
			Token::Return => todo!(),
			Token::Ident(_) => todo!(),
			Token::SpecialIdent(_) => todo!(),

			Token::Punc(punc) => match punc {
				Punc::Eq => {
					todo!()
				},
				Punc::Comma => {
					todo!()
				},
				Punc::LBrace => todo!(),
				Punc::RBrace => todo!(),
				Punc::LBracket => todo!(),
				Punc::RBracket => todo!(),
				Punc::LParen => todo!(),
				Punc::RParen => todo!(),
			},
		}
	}

	let file_type_str = file_type_str.expect("Error: no valuetype specified!");

	match file_type_str {
		"class" => todo!(),
		"function" => todo!(),
		"method" => todo!(),
		"value" => todo!(),
		"lib" => todo!(),
		"event" => todo!(),
		_ => panic!("bad type!!")
	}
}