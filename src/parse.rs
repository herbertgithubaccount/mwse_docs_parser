#![feature(let_chains)]
#![feature(type_alias_impl_trait)]

use nom::{
	branch::alt, bytes::complete::{is_not, take_until}, character::complete::{alpha1,  space0}, combinator::opt, error::Error as PError, number::{self, complete::{be_f64, be_i64}}, sequence::{self, delimited, preceded, terminated}, Parser
};

#[inline]
fn tag<'a>(text: &'static str) -> impl Fn(&'a str) -> Res<'a, &'a str> {
	nom::bytes::complete::tag(text)
}
#[inline]
fn is_a<'a>(text: &'static str) -> impl FnMut(&'a str) -> Res<'a, &'a str> {
	nom::bytes::complete::is_a(text)
}
#[inline]
fn ch<'a>(ch: char) -> impl FnMut(&'a str) -> nom::IResult<&'a str, char, PError<&'a str>> {
	nom::character::complete::char(ch)
}

type Res<'a, O> = nom::IResult<&'a str, O, PError<&'a str>>;

type BoxErr = Box<dyn std::error::Error>;


#[derive(Debug, Clone, Copy)]
enum Comment<'a> {
	Single(&'a str),
	Multi(&'a str)
}
/// Parses a comment (either single line or block).
/// If `--[[` is encounted without a closing `]]`, then this returns an error.
/// If no comment was encounted, then this returns an error.
pub fn comment(i: &str) -> Res<Comment> {

	let (i, _) = tag("--")(i)?;

	// If we can eat "[[", then parse it as a multi comment (possibly triggering an error)
	if let Ok((i, _)) = tag("[[")(i) {
		terminated(take_until("]]"), tag("]]"))
		.map(Comment::Multi)
		.parse(i)
	} else {
		is_not("\n")
		.map(Comment::Single)
		.parse(i)
	}
}


#[derive(Debug, Clone)]
enum Val<'a> {
	Str(&'a str),
	Int(i64),
	Num(f64),
	Table(Box<Table<'a>>),
	Bool(bool),
}

#[derive(Debug, Clone)]
struct TableEntry<'a>{
	key: &'a str,
	val: Val<'a>,
}



#[derive(Debug, Clone)]
struct Table<'a>(Vec<TableEntry<'a>>);

// macro_rules! ch {
// 	($char:lit) => {
// 		let i = char('{')
// 	};
// }
macro_rules! spaces {
	($p:expr) => {
		|x| {delimited(space0, $p, space0).parse(x) }
	};
}
macro_rules! parse {
	($i:ident => $($o:pat in $p:expr);* $(;)?) => {
		$(
			let ($i, $o) = $p($i)?;
		)*
	};
	($i:ident => $($p:expr);* $(;)?) => {
		$(
			let ($i, _) = $p($i)?;
		)*
	};
}

macro_rules! try_alt {
	($ty:ty; $($e:expr),+ $(,)?) => {
		{
			$(
				let res: $ty = try { $e };
				if let Ok(v) = res { return Ok(v) }
			)*
		}
		// $(
		// 	if let Ok::<$ty>(v) = try { $e } {return Ok(v)}
		// )*
		
	};
}

fn val<'a>(i: &'a str) -> Res<'a, Val<'a>> {
	try_alt!(Res<'a, Val<'a>>; 
		{
			parse!(i => tag("true"));
			(i, Val::Bool(true))
		}, {
			parse!(i => tag("false"));
			(i, Val::Bool(true))
		}, {
			parse!(i =>
				sgn in |x| opt(ch('-')).parse(x);
				num in is_a("123456789");
			);
			let sgn = sgn.unwrap_or('+');
			if let Ok((i, Some(_))) = opt(ch('.')).parse(i) 
			&& let Ok((i, num2)) = is_a("123456789").parse(i)
			{
				(i, Val::Num(format!("{sgn}{num}.{num2}").parse().unwrap()))
			} else if sgn == '+' {
				(i, Val::Int(num.parse().unwrap()))
			} else {
				(i, Val::Int(- (num.parse::<i64>().unwrap())))
			}
		}, {
			parse!(i =>
				_ in ch('"');
				s in is_not("\"");
				_ in ch('"');
			);
			(i, Val::Str(s))
		}
	);
	// nume
	// opt
// opt
	// if let Ok((i,n)) = be_i64::<_, PError<&[u8]>>(i.as_bytes()) {
	// 	return unsafe { 
	// 		Ok((std::str::from_utf8_unchecked(i), Val::Int(n)))
	// 	}
	// }
	// if let Ok((i,n)) = be_f64::<_, PError<&[u8]>>(i.as_bytes()) {
	// 	return unsafe { 
	// 		Ok((std::str::from_utf8_unchecked(i), Val::Num(n)))
	// 	}
	// }

	


	todo!()
}


fn parse_table<'a>(i: &'a str) -> Res<'a, Table<'a>> {
	// let mut i = i;
	// let (i, key) = alpha1(i)?;
	// let key: &str;
	parse!{i =>
		_ in spaces!(ch('{'));
		key in alpha1;
		_ in spaces!(ch('='));
	};


	todo!()

}

// fn parse_table<'a>(inp: &'a str) -> 

