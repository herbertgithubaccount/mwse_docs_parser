#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(guard_patterns)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![feature(yeet_expr)]
#![feature(try_trait_v2_yeet)]
#![feature(type_alias_impl_trait)]

mod lex;
type BoxErr = Box<dyn std::error::Error>;

mod parse;
pub use parse::*;



fn main() -> Result<(), BoxErr> {
	// let input = r#"abc = { hi "this is a literal string}"#;
	let input = include_str!("../docs/namedTypes/mwseMCMSlider/new.lua");

	use lex::SpecialIdent::*;
	// println!("lexing {input}");

	let tokens = lex::get_tokens(input);

	if let Ok(tokens) = tokens {
		println!("printing tokens!");
		for (i, t) in tokens.iter().enumerate() {
			println!("\t{i}) {t:?}")
		}
		dbg!(parse(&tokens));

	} else {
		println!("Got tokens = {tokens:?}");
	}


	Ok(())

}
