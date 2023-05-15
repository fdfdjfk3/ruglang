use crate::navigator::{Navigator, EOF, NEWLINE};

/*
pub enum LexerError {
    // Extra symbols error, holding a character of the first symbol that is wrong
    ExtraSymbols(String),
}
*/

/// Token parsed directly from the file.
/// Tokens on their own cannot express the layout of a program, that happens once the AST is created
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Ident {
        val: String,
    },
    Var,
    Function,
    As,
    Returns,
    Literal {
        lit_type: LiteralType,
    },
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

    EOF,

    #[allow(dead_code)]
    Whitespace,

    Unknown,

    Comment,
    // error values that will be handled later
    /*
    InvalidLiteral {
        lit_type: LiteralType,
        error_type: LexerError,
    },
    */
}

impl Token {
    pub fn can_be_operator(&self) -> bool {
        match self {
            Self::Plus => true,
            Self::Minus => true,
            Self::Asterisk => true,
            Self::Slash => true,
            _ => false,
        }
    }
    pub fn could_be_a_value(&self) -> bool {
        match self {
            Self::Literal { .. } => true,
            Self::Ident { .. } => true,
            _ => false,
        }
    }
    pub fn operator_precedence(&self) -> u8 {
        match self {
            t if *t == Self::Asterisk || *t == Self::Slash => 1,
            t if *t == Self::Plus || *t == Self::Minus => 2,
            _ => panic!("non-operator does not have a precedence"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LiteralType {
    Integer { val: i64 },
    Float { val: f64 },
    Str { val: String },
    Boolean { val: bool },
}

/// Create an iterator over ruglang tokens parsed from the file
pub fn tokenize(input: &str) -> Vec<Token> {
    let mut navigator = Navigator::new(input);
    std::iter::from_fn(move || {
        let token = navigator.read_token();
        if token != Token::EOF {
            Some(token)
        } else {
            None
        }
    })
    .collect()
}

impl Navigator<'_> {
    pub fn read_token(&mut self) -> Token {
        self.skip_whitespace();
        let token_kind = match self.bump() {
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

            EOF => Token::EOF,
            _ => Token::Unknown,
        };
        token_kind
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
        let start = self.position() as usize + 1;
        while self.first() != '"' && self.first() != EOF {
            self.bump();
        }
        self.bump();
        let end = self.position() as usize;
        if unicode_xid::UnicodeXID::is_xid_start(self.first()) {
            panic!("Parse error when parsing a string at line uhhhh actually idk lol ill implement errors later");
            /*
            return Token::InvalidLiteral {
                lit_type: LiteralType::Str {
                    val: self.yoink_to_string(start, end),
                },
                error_type: LexerError::ExtraSymbols(self.yoink_char(end + 1)),
            };
            */
        }
        Token::Literal {
            lit_type: LiteralType::Str {
                val: self.yoink_to_string(start, end),
            },
        }
    }
    /// Parse either an Integer or Float
    fn number(&mut self) -> Token {
        let mut dots = 0;
        let start = self.position() as usize;

        while !self.first().is_whitespace() {
            // could this possibly be a float?
            if self.first() == '.' {
                match self.second() {
                    c if c.is_ascii_digit() => dots += 1,
                    _ => panic!(
                        "error parsing number, theres a dot but its not a valid float helppppp"
                    ),
                }
            } else if self.first() == ';' {
                break;
            } else if !self.first().is_ascii_digit() {
                panic!("messed up number literal lmfaoo bruh :skull:");
            }
            self.bump();
        }
        let end = self.position() as usize + 1;

        match dots {
            0 => {
                let maybe_i64_value = self.yoink_to_i64(start, end);
                if let Err(e) = maybe_i64_value {
                    panic!("Error parsing Integer value at line bla bla bla: {}", e);
                }
                Token::Literal {
                    lit_type: LiteralType::Integer {
                        val: maybe_i64_value.unwrap(),
                    },
                }
            }
            1 => {
                let maybe_f64_value = self.yoink_to_f64(start, end);
                if let Err(e) = maybe_f64_value {
                    panic!("Error parsing Float value at line bla bla bla: {}", e);
                }
                Token::Literal {
                    lit_type: LiteralType::Float {
                        val: maybe_f64_value.unwrap(),
                    },
                }
            }
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
        let start = self.position() as usize;
        while unicode_xid::UnicodeXID::is_xid_continue(self.first()) {
            self.bump();
        }
        let end = self.position() as usize + 1;
        Token::Ident {
            val: self.yoink_to_string(start, end),
        }
    }
    fn boolean_or_ident(&mut self) -> Token {
        let start = self.position() as usize;
        while unicode_xid::UnicodeXID::is_xid_continue(self.first()) {
            self.bump();
        }
        let end = self.position() as usize + 1;
        let segment = self.yoink_to_string(start, end);
        match &segment[..] {
            "true" => Token::Literal {
                lit_type: LiteralType::Boolean { val: true },
            },
            "false" => Token::Literal {
                lit_type: LiteralType::Boolean { val: false },
            },
            "var" => Token::Var,
            "function" => Token::Function,
            "as" => Token::As,
            "returns" => Token::Returns,
            _ => Token::Ident {
                val: self.yoink_to_string(start, end),
            },
        }
    }
}
