use derive_more::{Debug, From, TryInto};
use tokio::task::JoinError;





use log::debug;

use crate::{lex::{Kw, Lit, Punc, Token}, Key, Val};






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

	UnexpectedEOI,
	InvalidNum(Box<str>),
}



#[derive(Debug, Clone, Default)]
pub enum ParseErr{
	/// Expected a table key, got something else.
	#[debug("We were expecting a table key, instead we got {_0:?}")]
	KeyExpected(Token),
	/// Expected a table value, got something else.
	#[debug("We were expecting a table value, instead we got {_0:?}")]
	ValExpected(Token),


	#[debug("PuncExpected{{expected: {_1:?}, got: '{_0:?}'}}")]
	PuncExpected(Token, Punc),
	
	

	// /// A literal showed up in a place it was not supposed to.
	// UnexpectedLit(Lit),
	
	// #[debug("InvalidLitType{{kw: {_0:?}, lit: '{_1:?}'}}")]
	// InvalidLitType(Kw, Lit),
	
	// EventDatumNameNotSpecified(Option<Token>),

	#[debug("End of Input")]
	#[default]
	EOI,

	


	/// This happens when we encounter a table key that has an unsupported value.
	/// E.g., a table indexed by booleans.
	/// E.g., we wanted a table but we got a number.
	#[debug("Got an unsupported literal when parsing keys: {_0:?}")]
	UnsupportedKeyType(Lit),

	
}

#[derive(Debug)]
pub enum DocFormatError {
	/// This happens when a certain kind of key shows up in a place it's not supposed to.
	/// E.g., when the produced documentation does not recognize this type of key.
	#[debug("Invalid key type given for {key:?}: {msg}")]
	UnexpectedKey{ key: Key, msg: &'static str },
	/// This happens when a certain kind of value shows up in a place it's not supposed to.
	/// E.g., we wanted a table but we got a number.
	#[debug("Invalid value type given for {val:?}: {msg}")]
	UnexpectedValue{ val: Val, msg: &'static str },

	/// A keyword showed up in a place it wasn't supposed to.
	#[debug("{kw:?} is not a keyword specifier for {tbl_name:?}")]
	UnsupportedKw{kw: Kw, tbl_name: &'static str},

	#[debug("Bad Package Type: {_0:?} is not supported.")]
	BadPkgTy(Box<str>),
}

impl Kw {
	/// Returns an error saying this keyword is invalid for the table with the given name.
	pub fn unsuported(self, tbl_name: &'static str) -> Result<!, DocFormatError> {
		Err(DocFormatError::UnsupportedKw { kw: self, tbl_name})
	}
}


#[derive(Debug, From, TryInto)]
pub enum Error {
	#[debug("Parsing Error: {_0:?}")]
	Parse(ParseErr),
	#[debug("Lexing Error: {_0:?}")]
	Lex(LexError),
	#[debug("IO Error: {_0:?}")]
	Io(std::io::Error),
	#[debug("Error: {_0:?}")]
	Msg(String),
	#[debug("Tokio Error: {_0:?}")]
	Join(JoinError),
	
	#[debug("Documentation Formatting Error: {_0:?}")]
	FmtErr(DocFormatError),
}


