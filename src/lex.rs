use std::pin::Pin;

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Lit {
	Str(Box<str>),
	Int(i64),
	Num(f64),
	Bool(bool)
}

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Token {
	Lit(Lit),
	Comma,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	LParen,
	RParen,
	Eq,
	Ident(Box<str>),
}

#[derive(Debug)]
pub struct Lexer<'a> {
	/// Input string
	input: &'a [u8],
	/// Line within the input.
	line: usize,
	/// Start of the line being processed.
	line_start: usize,
	/// Current position inside the input
	pos: usize,
	/// Start position of the token currently being processed
	token_start: usize,

	/// Tokens produced so far.
	tokens: Vec<Token>,
}

#[derive(Clone, Copy, Debug)]
pub enum LexError<'a> {
	/// Input ended before we could finish processing a token
	EndedEarly,
	UnexpectedChar(char),
	UnterminatedStr(&'a str),
	BadEscapeChar(char),
	/// We cannot process a token because we have already reached the end of the input.
	EndOfInput,
}



impl<'a> Lexer<'a> {
	pub fn new(input: &'a str) -> Self {
		// let ch = input[0];
		Self {
			input: input.as_bytes(),
			line: 0,
			line_start: 0,
			pos: 0,
			token_start: 0,
			tokens: Vec::with_capacity(input.len())
		}
	}

	fn advance(&mut self) -> Option<u8> {
		if let Some(ch) = self.peek() {
			self.pos += 1;
			Some(ch)
		} else {
			None
		}
	}
	fn peek(&self) -> Option<u8> {
		self.input.get(self.pos).copied()
	}
	fn peek2(&self) -> Option<u8> {
		self.input.get(self.pos + 1).copied()
	}

	pub fn scan_token(&mut self) -> Result<Token, LexError> {
		let token = try {
			loop {
				let Some(ch) = self.advance() else {
					do yeet LexError::EndOfInput
				};
				match ch {
					b'{' => break Token::LBracket,
					b'}' => break Token::RBracket,
					b']' => break Token::RBrace,
					b'(' => break Token::LParen,
					b'=' => break Token::Eq,
					b')' => break Token::RParen,
					// numbers
					b'0'..=b'9' => {
						while let Some(b'0'..=b'9') = self.peek() {
							self.advance();
						}
						if let Some(b'.') = self.advance() {
							while let Some(b'0'..=b'9') = self.peek() {
								self.advance();
							}
							let slice = &self.input[self.token_start..self.pos];
							let slice = unsafe {std::str::from_utf8_unchecked(slice) };
							break Token::Lit(Lit::Num(slice.parse().unwrap()))
						} else {
							let slice = &self.input[self.token_start..self.pos];
							let slice = unsafe {std::str::from_utf8_unchecked(slice) };
							break Token::Lit(Lit::Int(slice.parse().unwrap()))
						}
					},
					// string literals
					b'"' => {
						let mut bytes = Vec::new();
						loop {
							let Some(ch) = self.advance() else { 
								do yeet LexError::UnterminatedStr(
									unsafe {
										&std::str::from_utf8_unchecked(&self.input)[self.token_start..self.pos]
									}
								)
							};
							match ch {
								b'"' => break,
								b'\n' => do yeet LexError::UnterminatedStr(
									unsafe {
										&std::str::from_utf8_unchecked(&self.input)[self.token_start..self.pos]
									}
								),
								b'\\' => {
									let Some(ch) = self.advance() else {
										do yeet LexError::BadEscapeChar(ch as char)
									};
									match ch {
										b'"' | b'\\'  => bytes.push(ch),
										b'n' => bytes.push(b'\n'),
										b't' => bytes.push(b'\t'),
										b'z' => {
											while let Some(b'\t'|b'\n'|b' ') = self.peek() {
												self.advance();
											}
										},
										_ => do yeet LexError::BadEscapeChar(ch as char)
									}
								},
								_ => bytes.push(ch),
							}
						}
						let s = unsafe {std::str::from_boxed_utf8_unchecked(bytes.into_boxed_slice()) };
						break Token::Lit(Lit::Str(
							s
						))
					},
					first_char@(b'a'..=b'z' | b'A' ..= b'Z') => {
						let mut bytes = vec![first_char];
						while let Some(new_char@(b'a'..=b'z' | b'A' ..= b'Z')) = self.peek() {
							bytes.push(new_char);
							self.advance();
						}
						break Token::Ident(unsafe{
							std::str::from_boxed_utf8_unchecked(bytes.into_boxed_slice())
						})
					},		
					b' '|b'\t'|b'\n'|b'\r' => continue,
					b',' => break Token::Comma,
					b'[' => {
						if let Some(ch1) = self.peek() && ch1 != b'[' {
							break Token::LBrace;
						}
						self.advance();
						let mut bytes = Vec::new();
						loop {
							let Some(ch) = self.advance() else { 
								do yeet LexError::UnterminatedStr(
									unsafe {
										&std::str::from_utf8_unchecked(&self.input)[self.token_start..self.pos]
									}
								)
							};
							match ch {
								b']' if let Some(b']') = self.peek() => {
										self.advance();
										break;
								},
								_ => bytes.push(ch),
							}
						}
						let s = unsafe {std::str::from_boxed_utf8_unchecked(bytes.into_boxed_slice()) };
						break Token::Lit(Lit::Str(
							s
						))
						
					},

					_ => panic!("Unsupported input! {}", ch as char)
				}
			}
			
		};
		self.token_start = self.pos;
		token
	}
}