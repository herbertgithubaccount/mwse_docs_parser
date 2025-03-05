#![feature(let_chains)]
#![feature(type_alias_impl_trait)]
use std::os::unix::process::parent_id;

use nom::{
	bytes::complete::{is_not, tag, take_until}, character::complete::{alpha1, char, space0}, error::Error as PError, sequence::{self, delimited, preceded, terminated}, Parser
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


#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum Primitive {
	Bool,
	Int,
	Num,
	Str
}

impl TryFrom<&str> for ValueType {
	type Error = ();

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		if let Ok(prim) = Primitive::try_from(value) {
			Ok(Self::Primitive(prim))
		} else {
			match value {
				"class" => Ok(Self::Class),
				"table" => Ok(Self::Table),
				_ => Err(())
			}
		}

	}
}
#[derive(Debug, Clone, Copy, PartialEq)]
enum ValueType {
	Class,
	Primitive(Primitive),
	Table,
}

impl TryFrom<&str> for Primitive {
	type Error = ();

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		match value {
			"bool" | "boolean" => Ok(Self::Bool),
			"number" => Ok(Self::Num),
			"int" | "integer" => Ok(Self::Int),
			"string" => Ok(Self::Str),
			_ => Err(())
		}
	}
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


fn valuetype<'a>(inp: &'a str) -> Res<'a, ValueType> {
	let mut key = tag("valuetype");
	let mut valuetype = delimited(char('"'), alpha1, char('"'));
	let (i, (_, o)) =  key_value_pair!(key, valuetype).parse(inp)?;


	// .parse(inp)?
	if let Ok(vt) = ValueType::try_from(o) {
		return Ok((i, vt));
	}
	return Err(nom::Err::Error(nom::error::Error{
		input: inp,
		code: nom::error::ErrorKind::Alpha
	}));
	
}






// fn defn_start<'a, I, O, P>(i: &'a I, parser: P) -> impl Parser<I, Output = O>
// where
// 	P: Parser<I, Output = O>
// {

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
