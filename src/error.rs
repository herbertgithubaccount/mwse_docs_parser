use derive_more::{Debug, From, TryInto};
use tokio::task::JoinError;

use crate::{lex::LexError, ParseErr};

#[derive(Debug, From, TryInto)]
pub enum Error {
	Parse(ParseErr),
	Lex(LexError),
	Io(std::io::Error),
	Msg(String),
	Join(JoinError)
}