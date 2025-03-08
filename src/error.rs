use derive_more::{Debug, From, TryInto};
use tokio::task::JoinError;

use crate::{lex::LexError, ParseErr};

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
	Join(JoinError)
}