#![feature(let_chains)]
#![feature(type_alias_impl_trait)]
use std::os::unix::process::parent_id;

use nom::{
	branch::alt, bytes::complete::{is_not, tag, take_until}, character::complete::{alpha1, char, space0}, error::Error as PError, sequence::{self, delimited, preceded, terminated}, Parser
};



type Res<'a, O> = nom::IResult<&'a str, O, PError<&'a str>>;

type BoxErr = Box<dyn std::error::Error>;


trait ParseFn<'a, O>: 'a {
	fn eval(&self, inp: &'a str) -> Res<'a, O>;
}

impl<'a, O, F: Fn(&'a str) -> Res<'a, O> + 'a> 
ParseFn<'a, O> for F  
{
	#[inline(always)]
	fn eval(&self, inp: &'a str) -> Res<'a, O> {
		self(inp)
	}
}

#[derive(Debug, Clone, Copy)]
enum Comment<'a> {
	Single(&'a str),
	Multi(&'a str)
}


impl Comment<'_> {
	fn is_single(&self) -> bool { 
		match self {
			Comment::Single(_) => true,
			Comment::Multi(_) => false,
		}
	}
	#[inline]
	fn is_multi(&self) -> bool { !self.is_single() }
}

impl<'a> From<Comment<'a>> for &'a str {
	fn from(value: Comment<'a>) -> Self {
		match value {
			Comment::Single(s) => s,
			Comment::Multi(s) => s,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Value<'a> {
	Table,
	Custom(&'a str),
	Bool,
	Int,
	Num,
	Str
}
#[derive(Debug, Clone, Copy, PartialEq)]
enum ValueType<'a> {
	Value,
	Class,
	Method,
	SimpleFn,
	ComplexFn(&'a str)
}
/// Parses a comment (either single line or block).
/// If `--[[` is encounted without a closing `]]`, then this returns an error.
/// If no comment was encounted, then this returns an error.
fn comment(i: &str) -> Res<Comment> {

	let (i, _) = tag("--")(i)?;

	// If we can eat "[[", then parse it as a multi comment (possibly triggering an error)
	if let Ok((i, _)) = tag::<_, _, PError<&str>>("[[")(i) {
		terminated(take_until("]]"), tag("]]"))
		.map(Comment::Multi)
		.parse(i)
	} else {
		is_not("\n")
		.map(Comment::Single)
		.parse(i)
	}
}

macro_rules! key_value_pair {
	(ignore $key:ident, $value:ident) => {
		move |inp| {
			let i = space0(inp)?.0;
			let i = $key.parse(i)?.0;
			let i = delimited(space0, char('='), space0).parse(i)?.0;
			// let i = space0(i)?.0;
			// let i = char('=')(i)?.0;
			// let i = space0(i)?.0;
			let (i, valout) = $value.parse(i)?;
	
			Ok((i, valout))
		}
	};
	($key:ident, $value:ident) => {
		move |inp| {
			let i = space0(inp)?.0;
			let (i, keyout) = $key.parse(i)?;
			let i = delimited(space0, char('='), space0).parse(i)?.0;
			// let i = space0(i)?.0;
			// let i = char('=')(i)?.0;
			// let i = space0(i)?.0;
			let (i, valout) = $value.parse(i)?;
	
			Ok((i, (keyout, valout)) )
		}
	};
}


fn valuetype<'a>(inp: &'a str) -> Res<'a, ValueType<'a>> {

	let i = space0(inp)?.0;
	let i = tag("valuetype")(i)?.0;
	let i = space0(i)?.0;
	let i = char('=')(i)?.0;
	let i = space0(i)?.0;

	if let Ok((i, _)) = tag::<_, _, PError<&str>>("class")(i) {
		Ok((i, ValueType::Class))
	} else if let Ok((i, _)) = tag::<_, _, PError<&str>>("value")(i) {
		Ok((i, ValueType::Value))
	} else if let Ok((i, _)) = tag::<_, _, PError<&str>>("function")(i) {
		Ok((i, ValueType::SimpleFn))
	} else {
		let (i, _) = tag("fun")(i)?;
		let (i, desc) = is_not("\"")(i)?;
		Ok((i, ValueType::ComplexFn(desc)))
	}
}
fn value<'a>(inp: &'a str) -> Res<'a, Value<'a>> {

	let i = space0(inp)?.0;
	let i = tag("value")(i)?.0;
	let i = space0(i)?.0;
	let i = char('=')(i)?.0;
	let i = space0(i)?.0;

	let (i, kw) = alpha1(i)?;
	let val = match kw {
		"bool" | "boolean" => Value::Bool,
		"table" => Value::Table,
		"int" | "integer" => Value::Int,
		"num" | "number" => Value::Num,
		"string" => Value::Str,
		_ => Value::Custom(kw)
	};
	Ok((i, val))
}






// fn defn_start<'a, I, O, P>(i: &'a I, parser: P) -> impl Parser<I, Output = O>
// where
// 	P: Parser<I, Output = O>
// {

// }


// fn inherits()

struct ClassDefn<'a> {
	inherits: Option<&'a str>,
	name: &'a str,

}

// fn class_defn(inp: &str) -> ClassDefn {

// }


fn main() -> Result<(), BoxErr> {

	let input = "--this is a comment";

	let input2 = r#"--[[This is a multiline
	comment!]]
	other stuff
	"#;

	let input3 = "valuetype = \"class\"";

	dbg!(comment(input2)?);
	dbg!(valuetype(input3)?);
    println!("Hello, world!");

	Ok(())

}
