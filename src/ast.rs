use crate::lexer::{Lexeme, Token};
use crate::navigator::{EOF, NEWLINE};

use std::iter::Peekable;

type Ast = Vec<AstNode>;
type Block = Vec<AstNode>;

#[derive(Debug, Clone)]
pub enum AstNode {
    FunctionDecl(FunctionDecl),
    GlobalVarDecl(VarDecl),
    VarDecl(VarDecl),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    identifier: String,
    params: Vec<(String, Type)>,
    returntype: Type,
    block: Block,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    identifier: String,
    value: Expr,
    immutable: bool,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    condition: Expr,
    if_true: Block,
    if_false: Block,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    condition: Expr,
    block: Block,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(String, Type),
    Ident(String),
    FnCall(String, Vec<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
    Nothing,
    Custom(String),
}

pub struct Parser<'a> {
    pub lexemes: Peekable<Box<dyn Iterator<Item = Lexeme> + 'a>>,
    pub file_str: &'a str,
    // errors will be tracked sooon
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    /// peek the type of the next lexeme
    fn peek_type(&mut self) -> Token {
        self.lexemes.peek().map(|t| t.token).unwrap_or(Token::EOF)
    }
    /// returns a new Parser
    pub fn new(
        lexemes: Peekable<Box<dyn Iterator<Item = Lexeme> + 'a>>,
        file_str: &'a str,
    ) -> Parser<'a> {
        Parser {
            lexemes,
            file_str,
            errors: Vec::new(),
        }
    }
    /// checks what's ahead and returns it in an option. if the iterator returns None, this
    /// function returns None. if it's not same as expected, it emits an error. uses .next().
    fn expect(&mut self, expected: Token, context: &str) -> Option<Lexeme> {
        let next_type = self.peek_type();
        if next_type == expected {
            return self.lexemes.next();
        } else {
            let oopsie = self.lexemes.next();
            self.report_error(
                format!(
                    "{}: Expected token {:?}, found {:?} instead.",
                    context, expected, next_type,
                ),
                oopsie,
            );
            return None;
        }
    }
    /// only checks if the next token is not EOF, uses .next().
    fn expect_not_eof(&mut self, context: &str) -> Option<Lexeme> {
        let next_type = self.peek_type();
        if next_type != Token::EOF {
            return self.lexemes.next();
        } else {
            let oopsie = self.lexemes.next();
            self.report_error(format!("{}: Premature END-OF-FILE", context,), oopsie);
            return None;
        }
    }

    /// casually check if a lexeme matches the input. does not report an error if it doesn't match.
    /// uses .peek().
    fn next_is(&mut self, expected: Token) -> bool {
        let lexeme = self.lexemes.peek();
        if lexeme.is_none() || lexeme.unwrap().token != expected {
            return false;
        }
        true
    }
    /// parses a program and returns Ast (Vec<AstNode>)
    pub fn parse(&mut self) -> Ast {
        self.program()
    }
    /// gets the row and column of where a lexeme starts.
    fn row_and_col_of(&self, lexeme: Lexeme) -> (usize, usize) {
        let substr = &self.file_str[0..=lexeme.span.start];

        let row = substr.chars().filter(|ch| *ch == NEWLINE).count();
        let mut col = 0;
        if row != 0 {
            for (i, ch) in substr.chars().rev().enumerate() {
                if ch == NEWLINE {
                    break;
                }
                col = i;
            }
        } else {
            col = lexeme.span.start;
        }
        (row, col)
    }
    /// pushes an error with context to self.errors
    fn report_error(&mut self, error_string: String, lexeme: Option<Lexeme>) {
        let (row, col): (usize, usize);
        if lexeme.is_some() {
            (row, col) = self.row_and_col_of(lexeme.unwrap());
            self.errors
                .push(format!("{}:{} > {}", row, col, error_string));
        } else {
            self.errors.push(format!("EOF > {}", error_string));
        }
    }
    /// Pattern: an entire program lol
    fn program(&mut self) -> Ast {
        let mut ast: Ast = Vec::<AstNode>::new();

        while self.lexemes.peek().is_some() {
            match self.lexemes.next().unwrap().token {
                Token::Function => {
                    let func = self.function_declaration();
                    if func.is_some() {
                        ast.push(func.unwrap());
                    }
                }
                Token::Var => ast.push(self.global_variable_declaration()),
                _ => {}
            }
        }
        for e in &self.errors {
            println!("{}", e);
        }
        ast
    }
    /// Pattern: function %ident%(%ident with explicit type%, ...) ?returns %type%? { %block% }
    fn function_declaration(&mut self) -> Option<AstNode> {
        let identifier = self.ident()?;

        self.expect(
            Token::OpenParen,
            "Function parameter list requires '(' at the start",
        )?;

        let mut params: Vec<(String, Type)> = Vec::new();
        while !self.next_is(Token::CloseParen) {
            let param = self.ident_with_explicit_type();
            params.push(param?);
            if self.next_is(Token::Comma) {
                self.lexemes.next();
            }
        }

        self.expect(
            Token::CloseParen,
            "Function parameter list requires ')' at the end",
        )?;

        let mut returntype = Type::Nothing;
        if self.next_is(Token::Returns) {
            returntype = self.func_return_type()?;
        }
        self.expect(
            Token::OpenBracket,
            "Function block definition must be properly enclosed in curly brackets",
        )?;
        let block = self.block();
        self.expect(
            Token::CloseBracket,
            "Function block definition must be properly enclosed in curly brackets",
        )?;

        let fndecl = Some(AstNode::FunctionDecl(FunctionDecl {
            identifier,
            params,
            returntype,
            block,
        }));
        println!("{:?}", fndecl);
        fndecl
    }
    fn block(&mut self) -> Block {
        let mut block: Block = Vec::<AstNode>::new();

        while !self.next_is(Token::CloseBracket) && self.lexemes.peek().is_some() {}
        block
    }
    /// Pattern: returns %type%
    fn func_return_type(&mut self) -> Option<Type> {
        self.expect(Token::Returns, "Specifying function return type")?;
        self.type_identifier()
    }
    /// Pattern: %ident% as %type%
    fn ident_with_explicit_type(&mut self) -> Option<(String, Type)> {
        let ident = self.ident()?;
        let vartype = self.explicit_type()?;
        Some((ident, vartype))
    }
    /// Pattern: as %type%
    fn explicit_type(&mut self) -> Option<Type> {
        self.expect(Token::As, "Specifying an explicit type")?;
        let vartype = self.type_identifier();
        vartype
    }
    fn global_variable_declaration(&mut self) -> AstNode {
        unimplemented!()
    }

    /* single thingies */

    /// Pattern: %ident%
    fn ident(&mut self) -> Option<String> {
        let lexeme = self.expect(
            Token::Ident,
            "Ident required to name function declaration or variable declaration",
        )?;
        Some(self.file_str[lexeme.span.start..lexeme.span.end].to_owned())
    }
    /// Pattern: %type%
    fn type_identifier(&mut self) -> Option<Type> {
        let lexeme = self.expect_not_eof("Expected a datatype")?;
        match lexeme.token {
            Token::IntType => Some(Type::Int),
            Token::FloatType => Some(Type::Float),
            Token::BoolType => Some(Type::Bool),
            Token::StrType => Some(Type::Str),
            Token::NothingType => Some(Type::Nothing),
            _ => {
                self.report_error(
                    format!("Expected type, found {:?}", lexeme.token),
                    Some(lexeme),
                );
                None
            }
        }
    }
}
