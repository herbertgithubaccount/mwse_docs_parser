
use crate::lex::{Lit, Punc, SpecialIdent, Token};


use std::iter::Peekable;

#[derive(Debug, Clone, Copy)]
pub enum ParseErrType<'a>{
	BadToken(&'a Token<'a>),
	/// The value for this special identifier was not provided.
	UnspecifiedKeyword(SpecialIdent),
	PuncExpected(Punc),
	/// A special identifier showed up in a place it shouldn't have.
	UnsupportedKeyword(SpecialIdent),
}


// fn pa(iter: Peekabl)
// type TokenIter<'a> = std::iter::Peekable<impl Iterator<Item = &Token<'a>>>;
// Peekable;
#[derive(Debug, Clone, Copy)]
pub struct ParseErr<'a> {
	pub token: Option<&'a Token<'a>>,
	pub ty: ParseErrType<'a>,
}



// println!("expecting punc {}", stringify!($punc));
// 			match $iter.next() {
// 				Some(Token::Punc($punc)) => (println!("got punc {}", stringify!($punc))),
// 				maybe_token => do yeet ParseErr{token: maybe_token, ty: ParseErrType::PuncExpected($punc)}
// 			}
macro_rules! expect_punc {
	($iter:ident; $($punc:path),*) => {
		$(
			match $iter.next() {
				Some(Token::Punc($punc)) => (),
				maybe_token => do yeet ParseErr{token: maybe_token, ty: ParseErrType::PuncExpected($punc)}
			}
		)*
 	};
}

/// Eats a punctuation if possible. Returns `true` if it got eaten
fn munch_if_possible<'a>(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>, punc: Punc) -> bool {
	if let Some(Token::Punc(p)) = iter.peek() && *p == punc {
		iter.next();
		true
	} else {
		false
	}
}




pub trait FromTokens<'a>: Sized + 'a {
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>>;
}
#[derive(Debug, Clone, Copy)]
pub struct Example<'a> {
	pub path: &'a str,
	pub title: Option<&'a str>,
	pub description: Option<&'a str>,
}

/// Creates an example from the tokens. Consumes the path as well.
impl<'a> FromTokens<'a> for Example<'a> {

	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>> {
		let path = match iter.next() {
			Some(Token::Lit(Lit::Str(s))) => s,
			Some(Token::Ident(id)) => *id,
			Some(Token::Punc(Punc::LBrace)) => {
				let path = match iter.next() {
					Some(Token::Lit(Lit::Str(s))) => s,
					t =>  do yeet ParseErr{token: t, ty: ParseErrType::UnspecifiedKeyword(SpecialIdent::Title)},
				};
				expect_punc!(iter; Punc::RBrace);
				path
			},
			// Some(Token::SpecialIdent(special_ident)) => todo!(),
			t => do yeet ParseErr{token: t, ty: ParseErrType::PuncExpected(Punc::LBrace)}
		};
		println!("got path = {path}");
		expect_punc!(iter; Punc::Eq, Punc::LBracket);

		let mut title: Option<&str> = None;
		let mut description: Option<&str> = None;
		for _ in 1..=2 {
			match iter.next() {
				Some(Token::SpecialIdent(SpecialIdent::Title)) => {
					expect_punc!(iter; Punc::Eq);
					title = match iter.next() {
						Some(Token::Lit(Lit::Str(s))) => Some(s),
						t => do yeet ParseErr{token: t, ty: ParseErrType::UnspecifiedKeyword(SpecialIdent::Title)},
					};
				},
				Some(Token::SpecialIdent(SpecialIdent::Description)) => {
					expect_punc!(iter; Punc::Eq);
					description = match iter.next() {
						Some(Token::Lit(Lit::Str(s))) => Some(s),
						t => do yeet ParseErr{token: t, ty: ParseErrType::UnspecifiedKeyword(SpecialIdent::Description)},
					};
				},
				t => do yeet ParseErr{token: t, ty: ParseErrType::UnspecifiedKeyword(SpecialIdent::Title)},
			}
			dbg!(munch_if_possible(iter, Punc::Comma));
			dbg!(iter.peek());
			if let Some(Token::Punc(Punc::RBracket)) = iter.peek() {
				break
			}
		}
		expect_punc!(iter; Punc::RBracket);
		dbg!(iter.peek());

		dbg!(Ok(Example{path, title, description}))

	}
}


impl<'a> FromTokens<'a> for Vec<Example<'a>> {
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Vec<Example<'a>>, ParseErr<'a>> {
		
		expect_punc!(iter; Punc::LBracket);
		
		let mut examples = Vec::new();
		
		loop {

			examples.push(dbg!(Example::from_tokens(iter)?));
			munch_if_possible(iter, Punc::Comma);

			if let Some(Token::Punc(Punc::RBracket)) = dbg!(iter.peek()) {
				iter.next();
				break
			}
		};
		
		// expect_punc!(iter; Punc::RBracket);
		munch_if_possible(iter, Punc::Comma);

		Ok(examples)
	}
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
	valuetype: Option<&'a str>,
	// examples: Option<Vec<Example<'a>>>,

}


/// Stores an argument / return value of a function.
#[derive(Debug)]
pub struct FnArg<'a> {
	pub name: Option<&'a str>,
	pub ty: Option<&'a str>,
	pub optional: bool,
	pub description: Option<&'a str>,
	pub default: Option<&'a Lit<'a>>,
	pub table_params: Option<Vec<FnArg<'a>>>,
}


impl<'a> FromTokens<'a> for FnArg<'a> {
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>> {
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
			_ => do yeet ParseErr{token: Some(token), ty: ParseErrType::BadToken(token)},
		};
		println!("processing {sp:?}");

		expect_punc!(iter; Punc::Eq);

		if sp == SpecialIdent::TableParams {
			println!("\tprocessing table params....");
			table_params = Some(Vec::<FnArg>::from_tokens(iter)?);
			println!("\tgot table params = {table_params:?}");
		} else {
			// The literal to use when initializing the keyword
			let lit = match iter.next() {
				Some(Token::Lit(lit))  => lit,
				token => do yeet ParseErr{token, ty: ParseErrType::UnspecifiedKeyword(sp)}
			};

			match sp {
				SpecialIdent::Name => {
					let Lit::Str(s) = lit else {panic!("bad literal!")};
					name = Some(s);
					println!("\tgot name = {name:?}");
				},
				SpecialIdent::Description => {
					let Lit::Str(s) = lit else {panic!("bad literal!")};
					description = Some(s);
					println!("\tgot description = {description:?}");

				},
				SpecialIdent::Optional => {
					let Lit::Bool(b) = lit else {panic!("bad literal!")};
					optional = *b;
					println!("\tgot optional = {optional:?}");

				},
				SpecialIdent::Type => {
					let Lit::Str(s) = lit else {panic!("bad literal!")};
					ty = Some(s);
					println!("\tgot ty = {ty:?}");

				},
				SpecialIdent::Default => {
					default = Some(lit);
					println!("\tgot default = {default:?}");

				},
				SpecialIdent::TableParams => unreachable!("already processed table params!"),
				sp => do yeet ParseErr{token: Some(token), ty: ParseErrType::UnsupportedKeyword(sp)},
			}
		}
		
		// consume the next character
		// This should be a comma or bracket.
		// Otherwise, we error.
		match iter.next() {
			Some(Token::Punc(Punc::Comma)) => continue,
			Some(Token::Punc(Punc::RBracket)) => break,
			Some(t) => do yeet ParseErr{token: Some(t), ty: ParseErrType::BadToken(t)},
			None => do yeet ParseErr{token: None, ty: ParseErrType::PuncExpected(Punc::RBracket)}
		}

	}
	Ok(FnArg{
		name,
		description: description,
		ty,
		optional,
		default,
		table_params
	})

	}
}

impl<'a> FnArg<'a> {
	pub fn from_valuetype(value_type: Option<&'a str>) -> Self {
		Self{ 
			name: None, 
			ty: value_type, 
			optional: false, 
			description: None, 
			default: None, 
			table_params: None
		}
	}
}

impl<'a> FromTokens<'a> for Vec<FnArg<'a>> {

	/// Parses the list of function arguments, including the opening and closing braces
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Vec<FnArg<'a>>, ParseErr<'a>> {
		
		// eat the first bracket
		expect_punc!(iter; Punc::LBracket);

		// If there's no left bracket, then there's only a single argument.
		if !matches!(iter.peek(), Some(Token::Punc(Punc::LBracket))) {
			return Ok(vec![FnArg::from_tokens(iter)?]);
		}
		// eat the bracket
		// iter.next();

		let mut args = Vec::new();


		// While the next token is not a bracket
		while !matches!(iter.peek(), Some(Token::Punc(Punc::RBracket))) {
			args.push(dbg!(FnArg::from_tokens(iter))?);
			if let Some(Token::Punc(Punc::Comma)) = iter.peek() {
				iter.next();
			}
			
		}
		expect_punc!(iter; Punc::RBracket);

		Ok(args)
	}
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







#[derive(Debug)]
pub enum PackageType<'a> {
	Class(ClassPackage<'a>),
	Function(FunctionPackage<'a>),
	Method(FunctionPackage<'a>),
	Value(ValuePackage<'a>),
	Lib(LibPackage<'a>),
	Event(EventPackage<'a>)
}

#[derive(Debug)]
pub enum ValueType {

}


#[derive(Debug)]
pub struct Package<'a> {
	/// Name of the package.
	// pub name: &'a str,
	/// Description of the package.
	pub description: Option<&'a str>,
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
	pub examples:  Option<Vec<Example<'a>>>,
	///For libraries with sub-namespaces such as mwse.mcm, etc., this array contians the nested namespaces.
	pub libs: Option<Vec<Package<'a>>>,
}










// fn parse_example<'a>(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>, path: &'a str) -> Result<Example<'a>, ParseErr<'a>> {
// 	expect_punc!(iter; Punc::LBracket);
// 	let mut title = None;
// 	let mut description = None;
// }

impl<'a> FromTokens<'a> for Package<'a> {
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>> {
			
		// The value type string. Used to make sure we parsed the file correctly.
		let mut file_type_str: Option<&'a str> = None;



		let mut description: Option<&'a str> = None;
		let mut valuetype: Option<&'a str> = None;
		let mut experimental = false;
		let mut deprecated  = false;
		let mut examples:  Option<Vec<Example<'a>>> = None;
		let mut libs:  Option<Vec<Package<'a>>> = None;

		// function / method specific 
		let mut fn_args = Vec::new();
		let mut fn_rets = Vec::new();


		// class specific
		let mut is_abstract = false;
		let mut inherits: Option<&str> = None;

		// value specific
		let mut read_only = false;

		// let mut info = FileInfo{file_type: FileType::Class, description: None};

		// let mut iter = tokens.iter().peekable();
		
		expect_punc!(iter; Punc::Return, Punc::LBracket);

		let mut comma_or_bracket_wanted = false;

		while let Some(t) = iter.next() {
			println!("parsing token {t:?}");
			match t {

				Token::SpecialIdent(sp) => {
					comma_or_bracket_wanted = true;

					expect_punc!(iter; Punc::Eq);

					match *sp {
						SpecialIdent::Arguments => {
							fn_args = Vec::<FnArg>::from_tokens(iter)?;
							println!("parsed fn args = {fn_args:?}");
						},
						SpecialIdent::Returns => {
							// super hacky: backwards compatibility with old syntax
							if let Some(Token::Lit(Lit::Str(ret_name))) = iter.peek() {
								iter.next();
								fn_rets = vec![FnArg{ 
									name: Some(ret_name), 
									ty: None, 
									optional: false, 
									description: None, 
									default: None, 
									table_params: None 
								}]
							} else {
								fn_rets = Vec::<FnArg>::from_tokens(iter)?;
							}
							println!("parsed fn rets = {fn_args:?}");
						},
						SpecialIdent::Type => {
							if let Some(Token::Lit(Lit::Str(s))) = iter.next() {
								file_type_str = Some(s);
							} else {
								panic!("Invalid file type given!!");
							}
							// expect_punc!(iter; Punc::Comma);
						}
						SpecialIdent::Description => {
							if let Some(Token::Lit(Lit::Str(s))) = iter.next() {
								description = Some(s);
							} else {
								panic!("Invalid description!!");
							}
						},
						SpecialIdent::Examples => {
							examples = Some(Vec::<Example>::from_tokens(iter)?);
							println!("parsed examples! {examples:?}")
						},
						SpecialIdent::ValueType => {
							if let Some(Token::Lit(Lit::Str(s))) = iter.next() {
								valuetype = Some(s);
							} else {
								panic!("Invalid file type given!!");
							}
						},
						SpecialIdent::Class => todo!(),
						SpecialIdent::Inherits => {
							if let Some(Token::Lit(Lit::Str(s))) = iter.next() {
								inherits = Some(s);
							} else {
								panic!("Invalid file type given!!");
							}
							// munch_if_possible(iter, Punc::Comma);
						},
						SpecialIdent::IsAbstract => {
							if let Some(Token::Lit(Lit::Bool(b))) = iter.next() {
								is_abstract = *b;
							} else {
								panic!("Invalid file type given!!");
							}
							// munch_if_possible(iter, Punc::Comma);
						},
						SpecialIdent::ReadOnly => {
							if let Some(Token::Lit(Lit::Bool(b))) = iter.next() {
								read_only = *b;
							} else {
								panic!("Invalid file type given!!");
							}
						},
						_ => todo!("support special ident {sp:?}")
					}

				},
				
				Token::Lit(lit) => todo!(),
				Token::Ident(_) => todo!(),
				Token::SpecialIdent(_) => todo!(),

				Token::Punc(punc) => match punc {
					Punc::Eq => {
						todo!()
					},
					Punc::Comma => {
						if comma_or_bracket_wanted {
							comma_or_bracket_wanted = false;
							continue;
						} else {
							panic!("unexpected comma!");
						}
					},
					Punc::LBrace => todo!(),
					Punc::RBrace => todo!(),
					Punc::LBracket => todo!(),
					Punc::RBracket => {
						if comma_or_bracket_wanted {
							comma_or_bracket_wanted = false;
							continue;
						// TODO: this is a hacky fix that solves the problem of `COMMA, BRACKET` at the very end
						// of the file. but surely something better is possible.
						} else if iter.peek().is_some() {
							panic!("unexpected bracket!")
						}
					},
					Punc::LParen => todo!(),
					Punc::RParen => todo!(),
					_ => todo!()
				},
			}
		}

		let file_type_str = file_type_str.expect("Error: no valuetype specified!");

		// valuetype is sometimes used to specify function returns
		if valuetype.is_some() {
			if fn_rets.len() == 0 {
				fn_rets = vec![FnArg::from_valuetype(valuetype)];
			} else {
				assert!(fn_rets.len() == 1, "Error: Cannot specify both valuetype and returns");
				assert!(fn_rets[0].ty.is_none(), "Error: Cannot specify both valuetype and returns");
				fn_rets[0].ty = valuetype;
			}
		}
		// TODO: trigger error if value does not match parsed parameters
		let ty = match file_type_str {
			"class" => PackageType::Class(ClassPackage{is_abstract, inherits}),
			"function" => PackageType::Function(FunctionPackage{args: fn_args, rets: fn_rets}),
			"method" => PackageType::Method(FunctionPackage{args: fn_args, rets: fn_rets}),
			"value" => PackageType::Value(ValuePackage{read_only, valuetype}),
			"lib" => todo!(),
			"event" => todo!(),
			_ => panic!("bad type!!")
		};

		Ok(Package { description, deprecated, experimental, examples, libs, ty })
	}
}
