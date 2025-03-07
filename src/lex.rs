use std::cell::Cell;
use std::ops::FromResidual;
use std::{iter::Peekable, str::CharIndices};

use derive_more::Debug;
use std::borrow::Cow;

use log::{debug, trace};
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Lit<'a> {
	#[debug("{_0:?}")]
	Str(Cow<'a, str>),
	#[debug("{_0}")]
	Int(i64),
	#[debug("{_0}")]
	Num(f64),
	#[debug("{_0}")]
	Bool(bool)
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
	Libs,			// len = 4
	Link,			// len = 4

	Value, 			// len = 5
	Class, 			// len = 5
	Title,			// len = 5
	Links,			// len = 5,

	Filter,			// len = 6
	
	Default, 		// len = 7
	Returns, 		// len = 7
	
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
	ReturnType, 	// len = 10

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
				"libs" => Some(Self::Libs),
				"link" => Some(Self::Link),
				_ => None,
			},
			5 => match ident {
				"class" => Some(Self::Class),
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
				_ => None,
			},
			8 => match ident {
				"inherits" => Some(Self::Inherits),
				"examples" => Some(Self::Examples),
				"optional" => Some(Self::Optional),
				"readOnly" => Some(Self::ReadOnly),
				_ => None,
			},
			9 => match ident {
				"arguments" => Some(Self::Arguments),
				"blockable" => Some(Self::Blockable),
				"eventData" => Some(Self::EventData),
				"overloads" => Some(Self::Overloads),
				"valueType" => Some(Self::ValueType),
				"valuetype" => Some(Self::ValueType),
				"rightType" => Some(Self::RightType),
				_ => None,
			},
			10 => match ident {
				"deprecated" => Some(Self::Deprecated),
				"isAbstract" => Some(Self::IsAbstract),
				"returntype" => Some(Self::ReturnType),
				"resultType" => Some(Self::ResultType),
				_ => None,
			},
			11 => match ident {
				"description" => Some(Self::Description),
				"tableParams" => Some(Self::TableParams),
				_ => None,
			},
			12 => match ident {
				"experimental" => Some(Self::Experimental),
				_ => None,
			},
			_ => None,
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



#[derive(PartialEq, Debug)]
pub enum Token<'a> {
	Lit(Lit<'a>),
	Punc(Punc),
	Ident(&'a str),
	Kw(Kw),
}
// impl std::fmt::Display for Token<'_> {
// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// 		match self {
// 			Token::Lit(lit) => write!(f, "Lit({lit})"),
// 			Token::Punc(punc) => write!(f, "Punc({punc})"),
// 			Token::Ident(id) => write!(f, "Ident(\"{id}\")"),
// 			Token::Kw(kw) => write!(f, "Kw({kw:?})"),
// 		}
// 	}
// }

#[derive(Clone, Debug, Default)]
pub enum LexError {
	/// Input ended before we could finish processing a token
	UnterminatedMultiComment,
	UnexpectedChar(char),
	UnterminatedStr(Box<str>),
	BadEscapeChar(char),

	/// The input ended while we were expecting this character.
	ExpectedPunc(Punc),
	/// We cannot process a token because we have already reached the end of the input.
	#[default]
	EndOfInput,
}

impl std::fmt::Display for LexError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{self:?}")
	}
}

impl std::error::Error for LexError {}



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
fn parse_num<'a>(input: &'a str, iter: &mut Iter<'a>, start: usize) -> Lit<'a> {
	let mut end= start;
	while let Some((i, '0'..='9')) = iter.peek() {
		end = *i;
		iter.next();
	}
	if next_char_is(iter,'.') {
		end = iter.next().unwrap().0;
		while let Some((i, '0'..='9')) = iter.peek() {
			end = *i;
			iter.next();
		}

		let f = input[start..=end].parse().unwrap();
		Lit::Num(f)
	} else {
		let n = input[start..=end].parse().unwrap();
		Lit::Int(n)
	}
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
fn parse_str<'a>(input: &'a str, iter: &mut Iter<'a>, start: usize, sep: char) -> Result<Token<'a>, LexError> {
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
	Ok(Token::Lit(Lit::Str(Cow::from(chars))))
}

#[inline(always)]
fn scan_token_internal<'a>(input: &'a str, iter: &mut Iter<'a>) -> Result<Token<'a>, LexError> {

	// skip comments and whitespace
	
	let (start, first_char) = get_start(iter)?;
	trace!("\tlexing char {first_char}");
	match first_char {
		
		// numbers
		'0'..='9' => {
			Ok(Token::Lit(parse_num(input, iter, start)))
		},
		// negative numbers
		'-' if let Some('0'..='9') = peek_char(iter) => {
			let start = iter.next().unwrap().0;

			match parse_num(input, iter, start) {
				Lit::Int(n) => Ok(Token::Lit(Lit::Int(-n))),
				Lit::Num(f) => Ok(Token::Lit(Lit::Num(-f))),
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

			// check if it's a boolean
			if let Ok(b) = ident.parse() {
				return Ok(Token::Lit(Lit::Bool(b)));
			} 

			// otherwise, it's either a special identifier or a regular identifer
			match Kw::try_from_str(ident) {
				Some(sp) => Ok(Token::Kw(sp)),
				None => Ok(Token::Ident(ident))
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
								break Ok(Token::Lit(Lit::Str(Cow::Borrowed(&input[start..i]))))
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
pub struct TokenIter<'a> {
	tokens: Vec<Token<'a>>,
	/// Index of next token.
	idx: Cell<usize>,
}

impl<'a> TokenIter<'a> {

	pub fn from_input(input: &'a str) -> Result<TokenIter<'a>, LexError> {
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
			if let Token::Ident("return") = t {
				*t = Token::Punc(Punc::Return);
				break
			}
		}
		Ok(Self::new(tokens))
	}

	pub fn new(tokens: Vec<Token<'a>>) -> Self {
		Self {idx: Cell::new(0), tokens }
	}

	pub fn peek(&self) -> Option<&Token<'a>> {
		self.tokens.get(self.idx.get())
	}
	pub fn peek2(&self) -> Option<&Token<'a>> {
		self.tokens.get(1 + self.idx.get())
	}
	pub fn next(&self) -> Option<&Token<'a>> {
		let idx = self.idx.get();
		if let Some(t) = self.tokens.get(idx) {
			self.idx.set(idx + 1);
			Some(t)
		} else {
			None
		}
	}
}


