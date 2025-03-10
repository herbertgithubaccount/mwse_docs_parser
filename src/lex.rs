use std::cell::Cell;
use std::ops::FromResidual;
use std::{iter::Peekable, str::CharIndices};

use crate::result::{DocFormatError, ParseErr};
use crate::result::{LexError};

use derive_more::Debug;
use std::borrow::Cow;

use log::{debug, trace};
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Lit {
	#[debug("{_0:?}")]
	Str(Box<str>),
	// #[debug("{_0}")]
	// Int(i64),
	#[debug("{_0}")]
	Num(f64),
	#[debug("{_0}")]
	Bool(bool),
	Nil
}



impl Lit {
	// pub fn str_or_msg(self, msg: &'static str) -> Result<Box<str>, DocFormatError> {
	// 	match self {
	// 		Self::Str(s) => Ok(s),
	// 		lit => Err(DocFormatError::UnexpectedValue { val: lit.into(), msg})
	// 	}
	// }
}

// impl std::fmt::Display for Lit<'_> {
// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// 		match self {
// 			Lit::Str(val) => write!(f, "\"{val}\""),
// 			Lit::Int(val) => write!(f, "{val}"),
// 			Lit::Num(val) => write!(f, "{val}"),
// 			Lit::Bool(val) => write!(f, "{val}"),
// 		}
// 	}
// }


#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kw {

	Name, 			// len = 4
	Type, 			// len = 4
	Link,			// len = 4

	Value, 			// len = 5
	// Class, 			// len = 5
	Title,			// len = 5
	Links,			// len = 5,

	Filter,			// len = 6
	
	Default, 		// len = 7
	Returns, 		// len = 7
	/// Used in event documentation
	Related,		// len = 7
	
	Inherits, 		// len = 8
	Examples,		// len = 8
	Optional, 		// len = 8
	ReadOnly,		// len = 8
	
	Arguments, 		// len = 9
	Blockable,		// len = 9
	Overloads,		// len = 9
	EventData,		// len = 9
	ValueType, 		// len = 9
	/// Used in operator overloads
	RightType,		// len = 9
	
	Deprecated,		// len = 10
	IsAbstract, 	// len = 10

	/// Used in operator overloads
	ResultType,		// len = 10

	Description, 	// len = 11
	TableParams, 	// len = 11
	
	Experimental,	// len = 12
}

impl Kw {
	/// Tries to get a `SpecialIdent` from an ordinary identifier.
	/// Current implementation just does string comparisons, but 
	/// this might change to something more sophisticated later on, if needed
	#[inline(always)]
	pub fn try_from_str(ident: &str) -> Option<Self> {
		match ident.len() {
			4 => match ident {
				"name" => Some(Self::Name),
				"type" => Some(Self::Type),
				"link" => Some(Self::Link),
				_ => None,
			},
			5 => match ident {
				// "class" => Some(Self::Class),
				"value" => Some(Self::Value),
				"title" => Some(Self::Title),
				"links" => Some(Self::Links),
				_ => None,
			},
			6 => match ident {
				"filter" => Some(Self::Filter),
				_ => None,
			},
			7 => match ident {
				"default" => Some(Self::Default),
				"returns" => Some(Self::Returns),
				"related" => Some(Self::Related),
				_ => None,
			},
			8 => match ident {
				"inherits" => Some(Self::Inherits),
				"examples" => Some(Self::Examples),
				"optional" => Some(Self::Optional),
				"readOnly" => Some(Self::ReadOnly),
				"readonly" => Some(Self::ReadOnly),
				_ => None,
			},
			9 => match ident {
				"arguments" => Some(Self::Arguments),
				"blockable" => Some(Self::Blockable),
				"eventData" => Some(Self::EventData),
				"overloads" => Some(Self::Overloads),
				// "valueType" => Some(Self::ValueType),
				"valuetype" => Some(Self::ValueType),
				"rightType" => Some(Self::RightType),
				_ => None,
			},
			10 => match ident {
				"deprecated" => Some(Self::Deprecated),
				"isAbstract" => Some(Self::IsAbstract),
				"resultType" => Some(Self::ResultType),
				_ => None,
			},
			11 => match ident {
				"description" => Some(Self::Description),
				"tableParams" => Some(Self::TableParams),
				// "tableparams" => Some(Self::TableParams),
				_ => None,
			},
			12 => match ident {
				"experimental" => Some(Self::Experimental),
				_ => None,
			},
			_ => None,
		}
	}

	/// Gets a boxed string that contains the text corresponding to this keyword.
	#[inline(always)]
	pub fn boxed_str(self) -> Box<str> {
		match self {
			Self::Name => Box::from("name"),
			Self::Type => Box::from("type"),
			Self::Link => Box::from("link"),
			// Self::Class => Box::from("class"),
			Self::Value => Box::from("value"),
			Self::Title => Box::from("title"),
			Self::Links => Box::from("links"),
			Self::Filter => Box::from("filter"),
			Self::Default => Box::from("default"),
			Self::Returns => Box::from("returns"),
			Self::Related => Box::from("related"),
			Self::Inherits => Box::from("inherits"),
			Self::Examples => Box::from("examples"),
			Self::Optional => Box::from("optional"),
			Self::ReadOnly => Box::from("readOnly"),
			Self::Arguments => Box::from("arguments"),
			Self::Blockable => Box::from("blockable"),
			Self::EventData => Box::from("eventData"),
			Self::Overloads => Box::from("overloads"),
			Self::ValueType => Box::from("valueType"),
			Self::RightType => Box::from("rightType"),
			Self::Deprecated => Box::from("deprecated"),
			Self::IsAbstract => Box::from("isAbstract"),
			Self::ResultType => Box::from("resultType"),
			Self::Description => Box::from("description"),
			Self::TableParams => Box::from("tableParams"),
			Self::Experimental => Box::from("experimental"),
		}
	}
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Punc {
	#[debug(",")]
	Comma,
	#[debug("[")]
	LBrace,
	#[debug("]")]
	RBrace,
	#[debug("{{")]
	LBracket,
	#[debug("}}")]
	RBracket,
	#[debug("(")]
	LParen,
	#[debug(")")]
	RParen,
	#[debug("=")]
	Eq,
	#[debug("return")]
	Return,
}
// impl std::fmt::Display for Punc {
// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// 		match self {
// 				Punc::Comma => write!(f, ","),
// 				Punc::LBrace => write!(f, "["),
// 				Punc::RBrace => write!(f, "]"),
// 				Punc::LBracket => write!(f, "{{"),
// 				Punc::RBracket => write!(f, "}}"),
// 				Punc::LParen => write!(f, "("),
// 				Punc::RParen => write!(f, ")"),
// 				Punc::Eq => write!(f, "="),
// 				Punc::Return => write!(f, "return"),
// 			}
// 	}
// }



#[derive(PartialEq, Debug, Clone)]
pub enum Token{
	Lit(Lit),
	Punc(Punc),
	Ident(Box<str>),
	Kw(Kw),
}

impl Token {
	pub fn expect_lit(self) -> Lit {
		match self {
			Self::Lit(lit) => lit,
			t => panic!("Expected lit. Got {t:?}")
		}
	}
	pub fn expect_punc(self) -> Punc {
		match self {
			Self::Punc(inner) => inner,
			t => panic!("Expected punc. Got {t:?}")
		}
	}
	pub fn expect_ident(self) -> Box<str> {
		match self {
			Self::Ident(inner) => inner,
			t => panic!("Expected ident. Got {t:?}")
		}
	}
	pub fn expect_kw(self) -> Kw {
		match self {
			Self::Kw(inner) => inner,
			t => panic!("Expected keyword. Got {t:?}")
		}
	}
	pub fn lit_or<E>(self, f: impl Fn(Token) -> E) -> Result<Lit, E> {
		match self {
			Self::Lit(inner) => Ok(inner),
			t => Err(f(t))
		}
	}
	pub fn punc_or<E>(self, f: impl Fn(Token) -> E) -> Result<Punc, E> {
		match self {
			Self::Punc(inner) => Ok(inner),
			t => Err(f(t))
		}
	}
	pub fn kw_or<E>(self, f: impl Fn(Token) -> E) -> Result<Kw, E> {
		match self {
			Self::Kw(inner) => Ok(inner),
			t => Err(f(t))
		}
	}
	pub fn ident_or<E>(self, f: impl Fn(Token) -> E) -> Result<Box<str>, E> {
		match self {
			Self::Ident(inner) => Ok(inner),
			t => Err(f(t))
		}
	}
}


type Iter<'a> = Peekable<CharIndices<'a>>;

#[inline(always)]
fn peek_char<'a>(iter: &mut Iter<'a>) -> Option<char> {
	Some(iter.peek()?.1)
}

// Checks if the next character exists and is equal to a target character
fn next_char_is<'a>(iter: &mut Iter<'a>, target: char) -> bool {
	if let Some((_, ch)) = iter.peek() {
		*ch == target
	} else {
		false
	}
}

#[inline(always)]
fn next_char_is_not(iter: &mut Iter, target: char) -> bool {
	iter.peek().is_some_and(|(_, ch)| *ch != target)
}

#[inline(always)]
fn parse_num<'a>(input: &'a str, iter: &mut Iter<'a>, start: usize) -> Result<Lit, LexError> {
	let end = loop {
		match iter.peek() {
			Some((_, '0'..='9'|'-'|'+'|'e'|'x'|'X'|'A'..='E'|'b'|'.')) => {iter.next();}, 
			// won't e
			Some((i, _)) => break *i,
			None => do yeet LexError::UnexpectedEOI,
		}
	};
	match input[start..end].parse() {
		Ok(f) => Ok(Lit::Num(f)),
		Err(_) => do yeet LexError::InvalidNum(Box::from(&input[start..end]))
	}
	// let f = input[start..=end].parse()
	// loop {
	// 	match iter.peek() {
	// 		Some((i, 'e'))
	// 	}
	// }
	// while let Some((i, 'e'|'0'..='9')) = iter.peek() {
	// 	end = *i;
	// 	iter.next();
	// }
	// if next_char_is(iter,'.') {
	// 	end = iter.next().unwrap().0;
	// 	while let Some((i, 'e'|'0'..='9')) = iter.peek() {
	// 		end = *i;
	// 		iter.next();
	// 	}

	// 	let f = input[start..=end].parse().unwrap();
	// 	Lit::Num(f)
	// } else {
	// 	let n = input[start..=end].parse().unwrap();
	// 	Lit::Int(n)
	// }
}

/// Gets the start of the input, after skipping over comments and whitespace.
fn get_start<'a>(iter: &mut Iter<'a>) -> Result<(usize, char), LexError> {
	loop {
		let Some((i, ch)) = iter.next() else {
			do yeet LexError::EndOfInput;
		};

		match ch {
			' '|'\t'|'\n'|'\r' => (),
			'-' if next_char_is(iter, '-') => {
				iter.next(); // skip the next `-`
				// multiline comments
				if let Some((_, '[')) = iter.next() && next_char_is(iter, '[') {
					iter.next(); // eat the next `[`
					loop {
						let Some((_, ch)) = iter.next() else { do yeet LexError::UnterminatedMultiComment };

						if ch == ']' && next_char_is(iter, ']') {
							break // finished processing the comment
						}
					}
				} else {
					// single line comments
					loop {
						match iter.next() {
							None | Some((_, '\n')) => break,
							_ => (),
						}
					}
				}
			},

			_ => break Ok((i, ch))
		}
	}
}


#[inline(always)]
fn parse_str<'a>(input: &'a str, iter: &mut Iter<'a>, start: usize, sep: char) -> Result<Token, LexError> {
	// we'll need to rebuild the string in order to properly handle escape characters.
	let mut chars = String::new();
	loop {
		let Some((i, ch)) = iter.next() else { 
			do yeet LexError::UnterminatedStr(Box::from(&input[start..]))
		};
		if ch == sep {
			break
		} else if ch == '\n' {
			do yeet LexError::UnterminatedStr(Box::from(&input[start..=i]))
		} else if ch == '\\' {
			match iter.next() {
				None => do yeet LexError::EndOfInput,
				Some((_, ch2@('"'|'\\'|'\''))) => chars.push(ch2),
				Some((_, 't')) => chars.push('\t'),
				Some((_, 'n')) => chars.push('\n'),
				Some((_, 'z')) => {
					// println!("\tgot \\z escape character!");
					while let Some((_, ch2)) = iter.peek() && ch2.is_whitespace() {
						iter.next();
					}
				},
				Some((_, ch2)) => do yeet LexError::BadEscapeChar(ch2)
			}
		} else {
			chars.push(ch)
		}
	}
	Ok(Token::Lit(Lit::Str(chars.into_boxed_str())))
}

#[inline(always)]
fn scan_token_internal<'a>(input: &'a str, iter: &mut Iter<'a>) -> Result<Token, LexError> {

	// skip comments and whitespace
	
	let (start, first_char) = get_start(iter)?;
	trace!("\tlexing char {first_char}");
	match first_char {
		
		// numbers
		'0'..='9' => {
			Ok(Token::Lit(parse_num(input, iter, start)?))
		},
		// negative numbers
		'-' if let Some('0'..='9') = peek_char(iter) => {
			let start = iter.next().unwrap().0;

			match parse_num(input, iter, start) {
				// Ok(Lit::Int(n)) => Ok(Token::Lit(Lit::Int(-n))),
				Ok(Lit::Num(f)) => Ok(Token::Lit(Lit::Num(-f))),
				// Ok(Lit::Int(n)) => unreachable!("Unless I'm mistaken, parsing to integers is not currently being used."),
				_ => unreachable!("This should never happen")
			}
		},
		
		// identifiers and bools
		'a'..='z' | 'A'..='Z' => {

			// let mut end: usize = start;
			// while let Some((i, 'a'..='z' | 'A' ..= 'Z')) = iter.peek() {
			// 	end = *i;
			// 	iter.next();
			// }

			let end = loop {
				// Keep iterating until the next character is not a valid identifier character
				match iter.peek() {
					// An identifier cannot be the last input
					None => do yeet LexError::ExpectedPunc(Punc::Eq),
					Some((_, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')) => { iter.next(); },
					Some((i, _)) => break *i,
				}
			};
			
			let ident = &input[start..end];

			// check if it's a boolean or `nil`
			if let Ok(b) = ident.parse() {
				return Ok(Token::Lit(Lit::Bool(b)));
			} else if ident == "nil" {
				return Ok(Token::Lit(Lit::Nil))
			}

			// otherwise, it's either a special identifier or a regular identifer
			match Kw::try_from_str(ident) {
				Some(sp) => Ok(Token::Kw(sp)),
				None => Ok(Token::Ident(Box::from(ident)))
			}
		},
		'"' => parse_str(input, iter, start, '"'),	
		'\'' => parse_str(input, iter, start, '\''),	
		// Multiline string literals
		// In this case, we don't need to do any fancy manipulation on the string
		// Just find out where it ends and return the string slice.
		'[' if next_char_is(iter, '[') => {
			// we know the next char is a `[`, so might as well eat it.
			// we can add 1 because we know `[` is valid UTF8.
			let start = 1 + iter.next().unwrap().0;
			loop {

				match iter.next() {
					None => do yeet LexError::UnterminatedStr(Box::from(&input[start..])),
					Some((i, ']')) => {
						// println!("matched ']'");
						if let Some((_, ch2)) = iter.peek() {
							// println!("\tmatched ']'");
							if *ch2 == ']' {
								// println!("\tmatched a second']'");
								iter.next();
								break Ok(Token::Lit(Lit::Str(Box::from(&input[start..i]))))
							}
						}
					},
					_ => ()
				}
			}
		},
		// Regular punctuation
		'[' => Ok(Token::Punc(Punc::LBrace)),
		']' => Ok(Token::Punc(Punc::RBrace)),
		'{' => Ok(Token::Punc(Punc::LBracket)),
		'}' => Ok(Token::Punc(Punc::RBracket)),
		'(' => Ok(Token::Punc(Punc::LParen)),
		'=' => Ok(Token::Punc(Punc::Eq)),
		')' => Ok(Token::Punc(Punc::RParen)),
		',' => Ok(Token::Punc(Punc::Comma)),
		

		
		_ => Err(LexError::UnexpectedChar(first_char))
	}
}

#[derive(Debug)]
pub struct TokenIter {
	iter: std::vec::IntoIter<Token>,
}

impl TokenIter {

	pub fn from_input(input: &str) -> Result<TokenIter, LexError> {
		let mut tokens = Vec::new();

		debug!("lexing tokens of {input}");
		let mut iter = input.char_indices().peekable();

		loop {
			let res = scan_token_internal(input, &mut iter);
			match res {
				Ok(t) => {
					// println!("Parsed token {t:?}");
					tokens.push(t)
				},
				Err(LexError::EndOfInput) => break,
				Err(e) => return Err(e)
			}
		}
		// do the conversion now instead of when each token is created
		// because we probably only need to do this once
		for t in &mut tokens {
			if let Token::Ident(s) = t && s.as_ref() == "return" {
				*t = Token::Punc(Punc::Return);
				break
			}
		}
		Ok(Self::new(tokens))
	}

	pub fn new(tokens: Vec<Token>) -> Self {
		Self {iter: tokens.into_iter() }
	}

	pub fn peek(&self) -> Option<&Token> {
		self.iter.as_slice().get(0)
	}
	pub fn peek2(&self) -> Option<&Token> {
		self.iter.as_slice().get(1)
	}
	pub fn next(&mut self) -> Option<Token> {
		self.iter.next()
	}

	pub fn expect_punc(&mut self, to_be: Punc) -> Result<(), ParseErr> {
		match self.next() {
			None => return Err(ParseErr::EOI),
			Some(Token::Punc(p)) if p == to_be => Ok(()),
			Some(t) => Err(ParseErr::PuncExpected(t, to_be))
		}
	}

}


