use crate::navigator::{Navigator, EOF, NEWLINE};

/*
pub enum LexerError {
    // Extra symbols error, holding a character of the first symbol that is wrong
    ExtraSymbols(String),
}
*/

/// Token parsed directly from the file.
/// Tokens on their own cannot express the layout of a program, that happens once the AST is created
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Ident,
    Var,
    Function,
    As,
    Returns,

    StrLiteral,
    IntLiteral,
    FloatLiteral,
    BoolLiteral,

    StrType,
    IntType,
    FloatType,
    BoolType,
    NothingType,
    // =
    Equals,
    // +
    Plus,
    // -
    Minus,
    // /
    Slash,
    // *
    Asterisk,
    // ;
    Semicolon,
    // (
    OpenParen,
    // )
    CloseParen,
    // {
    OpenBracket,
    // }
    CloseBracket,
    // .
    Dot,
    // <
    LeftAngleBracket,
    // >
    RightAngleBracket,
    // ,
    Comma,

    EOF,

    Unknown,

    Comment,

    // only for use in the ast file, used as a wildcard
    Any,
}

/// Copyable range
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// Token parsed from source file
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Lexeme {
    pub token: Token,
    pub span: Span,
}

/// Create an iterator over ruglang tokens parsed from the file
pub fn tokenize(input: &str) -> Box<dyn Iterator<Item = Lexeme> + '_> {
    let mut navigator = Navigator::new(input);
    Box::new(std::iter::from_fn(move || {
        let lexeme = navigator.read_token();
        if lexeme.token != Token::EOF {
            Some(lexeme)
        } else {
            None
        }
    }))
}

impl Navigator<'_> {
    pub fn read_token(&mut self) -> Lexeme {
        self.skip_whitespace();
        let initial = self.bump();
        let start = self.position() as usize;
        let token_type = match initial {
            '/' => match self.first() {
                '/' => {
                    self.consume_until_newline();
                    Token::Comment
                }
                _ => Token::Slash,
            },
            c if Self::is_id_start(c) => self.boolean_or_ident(),

            '"' => self.quoted_string(),

            c if c.is_ascii_digit() => self.number(),

            '=' => Token::Equals,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            ';' => Token::Semicolon,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '{' => Token::OpenBracket,
            '}' => Token::CloseBracket,
            '.' => Token::Dot,
            '<' => Token::LeftAngleBracket,
            '>' => Token::RightAngleBracket,
            ',' => Token::Comma,

            EOF => Token::EOF,
            _ => Token::Unknown,
        };
        let end = self.position() as usize + 1;
        Lexeme {
            token: token_type,
            span: Span { start, end },
        }
    }
    /// Skip past the pesky whitespace.
    fn skip_whitespace(&mut self) {
        while self.first().is_whitespace() {
            self.bump();
        }
    }
    /// Eat characters until a whitespace is reached.
    fn _consume_until_whitespace(&mut self) {
        while !self.first().is_whitespace() && self.first() != EOF {
            self.bump();
        }
    }
    /// Collect quoted string if valid.
    fn quoted_string(&mut self) -> Token {
        while self.first() != '"' && self.first() != EOF {
            self.bump();
        }
        self.bump();
        if unicode_xid::UnicodeXID::is_xid_start(self.first()) {
            panic!("Parse error when parsing a string at line uhhhh actually idk lol ill implement errors later");
        }
        Token::StrLiteral
    }
    /// Parse either an Integer or Float
    fn number(&mut self) -> Token {
        let mut dots = 0;

        while !self.first().is_whitespace() {
            // could this possibly be a float?
            if self.first() == '.' {
                match self.second() {
                    c if c.is_ascii_digit() => dots += 1,
                    _ => panic!(
                        "error parsing number, theres a dot but its not a valid float helppppp"
                    ),
                }
            } else if !self.first().is_ascii_digit() {
                break;
            } /*else if Self::is_id_start(self.first()) {
                  panic!("messed up number literal lmfaoo bruh :skull:");
              }
              */
            self.bump();
        }

        match dots {
            0 => Token::IntLiteral,
            1 => Token::FloatLiteral,
            _ => panic!("uh oh invalid number !!!!"),
        }
    }
    fn consume_until_newline(&mut self) {
        while self.first() != NEWLINE {
            self.bump();
        }
        self.bump();
    }
    fn is_id_start(c: char) -> bool {
        unicode_xid::UnicodeXID::is_xid_start(c)
    }
    /// unused for now
    fn _ident(&mut self) -> Token {
        while unicode_xid::UnicodeXID::is_xid_continue(self.first()) {
            self.bump();
        }
        Token::Ident
    }
    fn boolean_or_ident(&mut self) -> Token {
        let start = self.position() as usize;
        while unicode_xid::UnicodeXID::is_xid_continue(self.first()) {
            self.bump();
        }
        let end = self.position() as usize + 1;
        let segment = self.yoink_to_string(start, end);
        match &segment[..] {
            "true" => Token::BoolLiteral,
            "false" => Token::BoolLiteral,
            "var" => Token::Var,
            "function" => Token::Function,
            "as" => Token::As,
            "returns" => Token::Returns,
            "Int" => Token::IntType,
            "Float" => Token::FloatType,
            "Bool" => Token::BoolType,
            "Str" => Token::StrType,
            "Nothing" => Token::NothingType,
            _ => Token::Ident,
        }
    }
}
