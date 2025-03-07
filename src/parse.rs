
use log::{debug, log_enabled, trace};

use crate::lex::{Lit, Punc, Kw, Token};
use derive_more::Debug;

use std::iter::Peekable;

#[derive(Debug, Clone, Copy)]
pub enum ParseErr<'a>{
	BadToken(&'a Token<'a>),
	/// The value for this special identifier was not provided.
	#[debug("UnspecifiedKeyword{{expected: {_1:?}, got: '{_0:?}'}}")]
	UnspecifiedKeyword(Option<&'a Token<'a>>, Kw),

	#[debug("PuncExpected{{expected: {_1:?}, got: '{_0:?}'}}")]
	PuncExpected(Option<&'a Token<'a>>, Punc),
	/// A special identifier showed up in a place it shouldn't have.
	#[debug("UnsupportedKeyword{{expected: {_1:?}, got: '{_0:?}'}}")]
	UnsupportedKeyword(Option<&'a Token<'a>>, Kw),
	BadPkgTy(&'a str),
	UnexpectedLit(&'a Lit<'a>),
	UnexpectedIdent(&'a str),
	#[debug("InvalidLitType{{kw: {_0:?}, lit: '{_1:?}'}}")]
	InvalidLitType(Kw, &'a Lit<'a>),

}

// impl<'a> std::fmt::Display for ParseErr<'a> {
// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// 		let token_str = match self {
// 			ParseErr::BadToken(token) => token.to_string(),
// 			ParseErr::UnspecifiedKeyword(Some(token),.. ) => token.to_string(),
// 			ParseErr::PuncExpected(Some(token),.. ) => token.to_string(),
// 			ParseErr::UnsupportedKeyword(Some(token),.. ) => token.to_string(),
// 			_ => String::from("None"),
// 		};
// 		match self {
// 			Self::BadToken(_) => write!(f, "BadToken({token_str})"),
// 			Self::UnspecifiedKeyword(_, kw) => write!(f, "UnspecifiedKeyword{{got: {token_str}, expected: {kw:?}}}"),
// 			Self::PuncExpected(_, punc) => write!(f, "PuncExpected{{got: {token_str}, expected: '{punc}')"),
// 			Self::UnsupportedKeyword(_, kw) => write!(f, "UnsupportedKeyword{{got: {token_str}, expected: {kw:?})"),
// 			Self::BadPkgTy(s) => write!(f, "BadPkgTy(\"{s}\""),
// 			Self::UnexpectedLit(lit) => match lit {
// 					Lit::Str(str) => write!(f, "UnexpectedLit({str})\""),
// 					Lit::Int(i) => write!(f, "UnexpectedLit({i})"),
// 					Lit::Num(num) => write!(f, "UnexpectedLit({num})"),
// 					Lit::Bool(b) => write!(f, "UnexpectedLit({b})"),
// 			},
// 			Self::UnexpectedIdent(id) => 
// 				write!(f, "UnexpectedIdent(\"{id}\")"),
// 		}
		
// 	}
// }




macro_rules! expect_punc {
	($iter:ident; $($punc:path),*) => {
		$(
			match $iter.next() {
				Some(Token::Punc($punc)) => (),
				maybe_token => do yeet ParseErr::PuncExpected(maybe_token, $punc)
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
					t =>  do yeet ParseErr::UnspecifiedKeyword(t, Kw::Title),
				};
				expect_punc!(iter; Punc::RBrace);
				path
			},
			// Some(Token::SpecialIdent(special_ident)) => todo!(),
			t => do yeet ParseErr::PuncExpected(t, Punc::LBrace)
		};
		trace!("got path = {path}");
		expect_punc!(iter; Punc::Eq, Punc::LBracket);

		let mut title: Option<&str> = None;
		let mut description: Option<&str> = None;
		for _ in 1..=2 {
			match iter.next() {
				Some(Token::Kw(Kw::Title)) => {
					expect_punc!(iter; Punc::Eq);
					title = match iter.next() {
						Some(Token::Lit(Lit::Str(s))) => Some(s),
						t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Title),
					};
				},
				Some(Token::Kw(Kw::Description)) => {
					expect_punc!(iter; Punc::Eq);
					description = match iter.next() {
						Some(Token::Lit(Lit::Str(s))) => Some(s),
						t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Description),
					};
				},
				t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Title),
			}
			munch_if_possible(iter, Punc::Comma);
			if log_enabled!(log::Level::Debug) {
				debug!("next token is {:?}", iter.peek());
			}
			if let Some(Token::Punc(Punc::RBracket)) = iter.peek() {
				break
			}
		}
		expect_punc!(iter; Punc::RBracket);
		debug!("next token is {:?}", iter.peek());

		Ok(Example{path, title, description})
	}
}


impl<'a> FromTokens<'a> for Vec<Example<'a>> {
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Vec<Example<'a>>, ParseErr<'a>> {
		
		expect_punc!(iter; Punc::LBracket);
		
		let mut examples = Vec::new();
		
		loop {

			examples.push(Example::from_tokens(iter)?);
			munch_if_possible(iter, Punc::Comma);

			if let Some(Token::Punc(Punc::RBracket)) = iter.peek() {
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
	pub read_only: bool,
	pub valuetype: Option<&'a str>,

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
				Token::Kw(sp)  => *sp,
				// End early if we found the closing bracket
				Token::Punc(Punc::RBracket) => break,
				_ => do yeet ParseErr::BadToken(token),
			};
			trace!("\t\tprocessing {sp:?}");

			expect_punc!(iter; Punc::Eq);

			if sp == Kw::TableParams {
				table_params = Some(Vec::<FnArg>::from_tokens(iter)?);
				trace!("\tgot table params = {table_params:?}");
			} else {
				// The literal to use when initializing the keyword
				let lit = match iter.next() {
					Some(Token::Lit(lit))  => lit,
					token => do yeet ParseErr::UnspecifiedKeyword(token, sp)
				};

				match sp {
					Kw::Name => {
						let Lit::Str(s) = lit else {panic!("bad literal!")};
						name = Some(s);
						trace!("\tgot name = {name:?}");
					},
					Kw::Description => {
						let Lit::Str(s) = lit else {panic!("bad literal!")};
						description = Some(s);
						trace!("\tgot description = {description:?}");

					},
					Kw::Optional => {
						let Lit::Bool(b) = lit else {panic!("bad literal!")};
						optional = *b;
						trace!("\tgot optional = {optional:?}");

					},
					Kw::Type => {
						let Lit::Str(s) = lit else {panic!("bad literal!")};
						ty = Some(s);
						trace!("\tgot ty = {ty:?}");

					},
					Kw::Default => {
						default = Some(lit);
						trace!("\tgot default = {default:?}");

					},
					Kw::TableParams => unreachable!("already processed table params!"),
					sp => do yeet ParseErr::UnsupportedKeyword(Some(token), sp),
				}
			}
			
			// consume the next character
			// This should be a comma or bracket.
			// Otherwise, we error.
			match iter.next() {
				Some(Token::Punc(Punc::Comma)) => continue,
				Some(Token::Punc(Punc::RBracket)) => break,
				Some(t) => do yeet ParseErr::BadToken(t),
				None => do yeet ParseErr::PuncExpected(None, Punc::RBracket)
			}

		}
		Ok(FnArg{ name, description, ty, optional, default, table_params })

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

	pub fn from_name(name: Option<&'a str>) -> Self {
		Self{ 
			name, 
			ty: None, 
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
		
		// Allow specifying the name using a literal string
		if let Some(Token::Lit(Lit::Str(ret_name))) = iter.peek() {
			iter.next();
			return Ok(vec![FnArg::from_name(Some(ret_name))]);
		}

		// Eat the first bracket
		expect_punc!(iter; Punc::LBracket);


		// Pick our path based on the first token
		match iter.peek() {
			// Expected path: do nothing and proceed with the rest of the function
			Some(Token::Punc(Punc::LBracket)) => {},

			// No arguments specified, return empty vector
			Some(Token::Punc(Punc::RBracket)) => {
				iter.next();
				return Ok(vec![]);
			},
			
			// Only one argument was specified, parse it normally.
			Some(_) => {
				return Ok(vec![FnArg::from_tokens(iter)?]);
			},
			None => do yeet ParseErr::UnspecifiedKeyword(None, Kw::Returns),
		}


		

		// If there's no left bracket, then there's only a single argument.
		if !matches!(iter.peek(), Some(Token::Punc(Punc::LBracket))) {
			return Ok(vec![FnArg::from_tokens(iter)?]);
		}
		// eat the bracket
		// iter.next();

		let mut args = Vec::new();


		// While the next token is not a bracket
		while !matches!(iter.peek(), Some(Token::Punc(Punc::RBracket))) {
			args.push(FnArg::from_tokens(iter)?);
			if log_enabled!(log::Level::Trace) {
				trace!("added function arguments: {:?}", args.last())
			}
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
	pub link: Option<&'a str>,
	///For libraries with sub-namespaces such as mwse.mcm, etc., this array contians the nested namespaces.
	pub sublibs: Option<Vec<LibPackage<'a>>>,
	// pub rets: Vec<FnArg<'a>>,
}

#[derive(Debug)]
pub struct EventDatum<'a> {
	pub name: &'a str,
	pub ty: Option<&'a str>,
	pub read_only: bool,
	pub description: Option<&'a str>
}

impl<'a> FromTokens<'a> for EventDatum<'a> {
	/// Parses stuff like
	/// ```lua
	///	["activator"] = {
	///		type = "tes3reference",
	///		readOnly = true,
	///		description = "The actor attempting to trigger the event.",
	///	},
	/// ```
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>> {
		let name = match iter.next() {
			Some(Token::Lit(Lit::Str(s))) => s,
			Some(Token::Ident(id)) => *id,
			Some(Token::Punc(Punc::LBrace)) => {
				let path = match iter.next() {
					Some(Token::Lit(Lit::Str(s))) => s,
					t =>  do yeet ParseErr::UnspecifiedKeyword(t, Kw::Name),
				};
				expect_punc!(iter; Punc::RBrace);
				path
			},
			t => do yeet ParseErr::PuncExpected(t, Punc::LBrace)
		};
		expect_punc!(iter; Punc::Eq, Punc::LBracket);
		let mut ty: Option<&str> = None;
		let mut read_only = false;
		let mut description: Option<&str> = None;

		loop {
			match iter.peek() {
				Some(Token::Punc(Punc::RBracket)) => break,
				Some(Token::Kw(Kw::Type)) => {
					iter.next();
					expect_punc!(iter; Punc::Eq);
					match iter.next() {
						Some(Token::Lit(Lit::Str(t)))  => ty = Some(t),
						t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Type),
					};
				},
				Some(Token::Kw(Kw::ReadOnly)) => {
					iter.next();
					expect_punc!(iter; Punc::Eq);
					match iter.next() {
						Some(Token::Lit(Lit::Bool(b)))  => read_only = *b,
						t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::ReadOnly),
					};
				},
				Some(Token::Kw(Kw::Description)) => {
					iter.next();
					expect_punc!(iter; Punc::Eq);
					match iter.next() {
						Some(Token::Lit(Lit::Str(desc)))  => description = Some(desc),
						t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Description),
					};
				},
				Some(&t) => do yeet ParseErr::UnspecifiedKeyword(Some(t), Kw::EventData),
				None => do yeet ParseErr::UnspecifiedKeyword(None, Kw::EventData),
			}
			munch_if_possible(iter, Punc::Comma);
		}
		iter.next();

		Ok(Self{name, ty, read_only, description})
		
	}
}
impl<'a> FromTokens<'a> for Vec<EventDatum<'a>> {
	/// Parses stuff like
	/// ```lua
	/// eventData = {
	///		["activator"] = {
	///			type = "tes3reference",
	///			readOnly = true,
	///			description = "The actor attempting to trigger the event.",
	///		},
	///		["target"] = {
	///			type = "tes3reference",
	///			readOnly = true,
	///			description = "The reference that is being activated.",
	///		},
	/// }
	/// ```
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>> {
		expect_punc!(iter; Punc::LBracket);
		
		let mut event_data = Vec::new();
		
		loop {

			event_data.push(dbg!(EventDatum::from_tokens(iter)?));
			munch_if_possible(iter, Punc::Comma);

			if let Some(Token::Punc(Punc::RBracket)) = dbg!(iter.peek()) {
				iter.next();
				break
			}
		};
		
		// expect_punc!(iter; Punc::RBracket);
		munch_if_possible(iter, Punc::Comma);

		Ok(event_data)
	}
}

#[derive(Debug)]
pub struct EventLink<'a> {
	pub name: &'a str,
	pub path: &'a str,
}

impl<'a> FromTokens<'a> for EventLink<'a> {
	/// parses something like 
	/// ```lua
	///# links = {
	///		["xActivate"] = "mwscript/functions/actor/xActivate",
	///# }
	/// ```
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>> {
		
		
		let name = match iter.next() {
			Some(Token::Lit(Lit::Str(s))) => s,
			Some(Token::Ident(id)) => *id,
			Some(Token::Punc(Punc::LBrace)) => {
				let path = match iter.next() {
					Some(Token::Lit(Lit::Str(s))) => s,
					t =>  do yeet ParseErr::UnspecifiedKeyword(t, Kw::Name),
				};
				expect_punc!(iter; Punc::RBrace);
				path
			},
			t => do yeet ParseErr::PuncExpected(t, Punc::LBrace)
		};

		expect_punc!(iter; Punc::Eq);
		let path = match iter.next() {
			Some(Token::Lit(Lit::Str(path))) => path,
			token => do yeet ParseErr::UnspecifiedKeyword(token, Kw::Link)
		};

		Ok(Self{name, path})
	}
}

impl<'a> FromTokens<'a> for Vec<EventLink<'a>> {
	/// parses something like 
	/// ```lua
	///	links = {
	///		["xActivate"] = "mwscript/functions/actor/xActivate",
	///	}
	/// ```
	fn from_tokens(iter: &mut Peekable<impl Iterator<Item = &'a Token<'a>>>) -> Result<Self, ParseErr<'a>> {
		expect_punc!(iter; Punc::LBracket);
		
		let mut links = Vec::new();

		loop {
			match iter.peek() {
				Some(Token::Punc(Punc::RBracket)) => break,
				Some(_) => { links.push(EventLink::from_tokens(iter)?); },
				None => do yeet ParseErr::UnsupportedKeyword(None, Kw::Links)
			}
			munch_if_possible(iter, Punc::Comma);
		}

		expect_punc!(iter; Punc::RBracket);
		Ok(links)
	}

}
#[derive(Debug)]
pub struct EventPackage<'a> {
	pub filter: Option<&'a str>,
	pub blockable: bool,
	pub event_data: Vec<EventDatum<'a>>,
	pub links: Vec<EventLink<'a>>,

	
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

		// function / method specific 
		let mut fn_args = Vec::new();
		let mut fn_rets = Vec::new();


		// class specific
		let mut is_abstract = false;
		let mut inherits: Option<&str> = None;

		// value specific
		let mut read_only = false;

		// libs specific
		let mut link: Option<&str> = None;

		// event specific
		let mut filter: Option<&str> = None;
		let mut blockable = false;
		let mut event_data = Vec::new();
		let mut links = Vec::new();

		// let mut info = FileInfo{file_type: FileType::Class, description: None};

		// let mut iter = tokens.iter().peekable();
		
		expect_punc!(iter; Punc::Return, Punc::LBracket);

		let mut comma_or_bracket_wanted = false;

		while let Some(t) = iter.next() {
			trace!("parsing token {t:?}");
			match t {

				Token::Kw(sp) => {
					comma_or_bracket_wanted = true;

					expect_punc!(iter; Punc::Eq);
					trace!("\tparsing {t:?} as keyword....");
					match *sp {
						Kw::Arguments => {
							fn_args = Vec::<FnArg>::from_tokens(iter)?;
							trace!("\tparsed fn args = {fn_args:?}");
						},
						Kw::Returns => {
							fn_rets = Vec::<FnArg>::from_tokens(iter)?;
							trace!("\tparsed fn rets = {fn_args:?}");
						},
						
						Kw::Examples => {
							examples = Some(Vec::<Example>::from_tokens(iter)?);
							debug!("got examples: {examples:?}");
						},
						Kw::EventData => {
							event_data = Vec::<EventDatum>::from_tokens(iter)?;
						},

						Kw::Type => {
							match iter.next() {
								Some(Token::Lit(Lit::Str(s))) => file_type_str = Some(s),
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Type)
							}
							// expect_punc!(iter; Punc::Comma);
						}
						Kw::Description => {
							match iter.next() {
								Some(Token::Lit(Lit::Str(s))) => description = Some(s),
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Description)
							}
						},
						Kw::ValueType => {
							match iter.next() {
								Some(Token::Lit(Lit::Str(s))) => valuetype = Some(s),
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::ValueType)
							}
						},
						Kw::Class => todo!(),
						Kw::Inherits => {
							match iter.next() {
								Some(Token::Lit(Lit::Str(s))) => inherits = Some(s),
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Inherits)
							}
						},
						
						Kw::Link => {
							match iter.next() {
								Some(Token::Lit(Lit::Str(s))) => link = Some(s),
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Link)
							}
						},
						Kw::Links => {
							links = Vec::<EventLink>::from_tokens(iter)?;
						},

						Kw::IsAbstract => {
							match iter.next() {
								Some(Token::Lit(Lit::Bool(b))) => is_abstract = *b,
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::IsAbstract)
							}
						},
						Kw::ReadOnly => {
							match iter.next() {
								Some(Token::Lit(Lit::Bool(b))) => read_only = *b,
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::ReadOnly)
							}
						},

						Kw::Deprecated => {
							match iter.next() {
								Some(Token::Lit(Lit::Bool(b))) => deprecated = *b,
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Deprecated)
							}
						},
						Kw::Experimental => {
							match iter.next() {
								Some(Token::Lit(Lit::Bool(b))) => experimental = *b,
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Experimental)
							}
						},
						Kw::Blockable => {
							match iter.next() {
								Some(Token::Lit(Lit::Bool(b))) => blockable = *b,
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Blockable)
							}
						},
						Kw::Filter => {
							match iter.next() {
								Some(Token::Lit(Lit::Str(s))) => filter = Some(s),
								t => do yeet ParseErr::UnspecifiedKeyword(t, Kw::Filter)
							}
						},
						
						sp => todo!("Support kw: {sp:?}")
					}

				},
				
				Token::Lit(lit) => do yeet ParseErr::UnexpectedLit(lit), 
				Token::Ident(id) => do yeet ParseErr::UnexpectedIdent(id), 

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
		debug!("returning packagetype {file_type_str}");
		// TODO: trigger error if value does not match parsed parameters
		let ty = match file_type_str {
			"class" => PackageType::Class(ClassPackage{is_abstract, inherits}),
			"function" => PackageType::Function(FunctionPackage{args: fn_args, rets: fn_rets}),
			"method" => PackageType::Method(FunctionPackage{args: fn_args, rets: fn_rets}),
			"value" => PackageType::Value(ValuePackage{read_only, valuetype}),
			"lib" => PackageType::Lib(LibPackage{link, sublibs: None}),
			"event" => PackageType::Event(EventPackage{filter, blockable, event_data, links}),
			_ => do yeet ParseErr::BadPkgTy(file_type_str)
			// _ => do yeet ParseErr{token}
		};

		Ok(Package { description, deprecated, experimental, examples, ty })
	}
}
