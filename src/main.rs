#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![feature(yeet_expr)]
#![feature(try_trait_v2_yeet)]
#![feature(type_alias_impl_trait)]
use std::{os::unix::process::parent_id, pin::pin};

use lex::Lexer;
use nom::{
	branch::alt, bytes::complete::{is_not, tag, take_until}, character::complete::{alpha1, char, space0}, error::Error as PError, sequence::{self, delimited, preceded, terminated}, Parser
};

mod lex;
type BoxErr = Box<dyn std::error::Error>;

mod parse;
pub use parse::*;



fn main() -> Result<(), BoxErr> {
	let input = r#"abc = { hi "this is a literal string}"#;
	let input = include_str!("../docs/namedTypes/mgeCameraConfig/fov.lua");
	let mut peakable = input.char_indices().peekable();

	dbg!(&input[0..7]);
	dbg!(peakable.peek());
	dbg!(peakable.peek());
	dbg!(peakable.next());
	dbg!(peakable.peek());
	dbg!(peakable.next());

	return Ok(());

	println!("lexing {input}");
	let mut lexer = Lexer::new(input);

	let mut tokens = vec![];
	loop {
		let t = lexer.scan_token();
		if let Ok(t) = t {
			tokens.push(t);
		} else {
			println!("got bad token! {t:?}");
			break
		}
	}
	dbg!(tokens);

	// let input = "--this is a comment";

	// let input2 = r#"--[[This is a multiline
	// comment!]]
	// other stuff
	// "#;

	// let input3 = "valuetype = \"class\"";

	// dbg!(comment(input2)?);
	// dbg!(valuetype(input3)?);
    // println!("Hello, world!");

	Ok(())

}
