use crate::lexer::{Lexeme, Token};
use crate::navigator::{EOF, NEWLINE};
use crate::utils::*;

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
    ident: String,
    value: Expr,
    mutable: bool,
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

    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
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

#[derive(PartialEq)]
enum ParseState {
    Recovering,
    Ok,
}

pub struct Parser<'a> {
    pub lexemes: Peekable<Box<dyn Iterator<Item = Lexeme> + 'a>>,
    pub file_str: &'a str,
    // errors will be tracked sooon
    pub errors: Vec<String>,
    state: ParseState,
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
            state: ParseState::Ok,
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
        self.state = ParseState::Recovering;
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
            let lexeme = self.lexemes.next();
            match lexeme.unwrap().token {
                Token::Function => {
                    self.state = ParseState::Ok;
                    let func = self.function_declaration();
                    if func.is_some() {
                        ast.push(func.unwrap());
                    }
                }
                Token::Var => {
                    self.state = ParseState::Ok;
                    ast.push(self.global_variable_declaration());
                }
                token => {
                    if self.state == ParseState::Ok {
                        self.report_error(
                            format!("Unknown token {:?} found at top level", token),
                            lexeme,
                        );
                    }
                }
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
        let block = self.block()?;
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
    /// a block of statements
    fn block(&mut self) -> Option<Block> {
        let mut block: Block = Vec::<AstNode>::new();
        while !self.next_is(Token::CloseBracket) && self.lexemes.peek().is_some() {
            let lexeme = self.lexemes.peek().copied().unwrap();
            match lexeme.token {
                Token::Var => {
                    self.lexemes.next();
                    let variable = self.variable_declaration();
                    if variable.is_some() {
                        block.push(variable.unwrap());
                    }
                }
                Token::Semicolon => {
                    self.lexemes.next();
                }
                token => {
                    if self.state == ParseState::Ok {
                        self.report_error(
                            format!("expected statement, found {:?}", token),
                            Some(lexeme),
                        );
                    }
                }
            }
        }
        Some(block)
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
    fn variable_declaration(&mut self) -> Option<AstNode> {
        let ident = self.ident()?;
        self.expect(
            Token::Set,
            "Variable declaration, '=' sign is required to assign an expression to a value",
        );
        let expr = self.expression()?;

        Some(AstNode::VarDecl(VarDecl {
            ident,
            value: expr,
            mutable: true,
        }))
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

    /// Pattern: a literal, use your imagination
    fn literal(&mut self) -> Option<Expr> {
        let lexeme = self.expect_not_eof("Literal expected")?;
        let datatype = match lexeme.token {
            Token::IntLiteral => Type::Int,
            Token::FloatLiteral => Type::Float,
            Token::BoolLiteral => Type::Bool,
            Token::StrLiteral => Type::Str,
            r#type => panic!("encountered unknown data type: {:?}", r#type),
        };
        let representation = self.file_str[lexeme.span.start..lexeme.span.end].to_owned();
        Some(Expr::Literal(representation, datatype))
    }

    /* expression stuff vvvv */

    fn expression(&mut self) -> Option<Expr> {
        self.expression_recursive(0)
    }

    fn expression_recursive(&mut self, min_binding_power: u8) -> Option<Expr> {
        let mut lside = match self.peek_type() {
            t if is_prim_literal(t) => self.literal()?,

            Token::Ident => self.ident_or_fncall_expr()?,
            Token::EOF => {
                self.report_error(format!("hit eof in expression"), None);
                return None;
            }
            _ => {
                let lexeme = self.lexemes.next().unwrap();
                self.report_error(format!("invalid token in expression"), Some(lexeme));
                return None;
            }
        };

        loop {
            let op = match self.peek_type() {
                Token::EOF => {
                    self.report_error(
                        "expected semicolon or continuation of expression".into(),
                        None,
                    );
                    None
                }
                Token::Semicolon => break,
                t if is_binary_op(t) => {
                    let lexeme = self.lexemes.peek().unwrap();
                    let op = token_to_binop(lexeme.token);
                    Some(op)
                }
                _ => {
                    let lexeme = self.lexemes.next();
                    self.report_error(
                        "Expected semicolon or continuation of expression".into(),
                        lexeme,
                    );
                    None
                }
            }?;

            let (lbp, rbp) = infix_binding_power(op);
            if lbp < min_binding_power {
                break;
            }

            self.lexemes.next();
            let rside = self.expression_recursive(rbp)?;

            lside = Expr::BinOp(op, Box::new(lside), Box::new(rside));
        }
        Some(lside)
    }

    /// this function parses either an ident or a function call
    fn ident_or_fncall_expr(&mut self) -> Option<Expr> {
        let ident = self.ident()?;
        if !self.next_is(Token::OpenParen) {
            return Some(Expr::Ident(ident));
        }
        self.expect(
            Token::OpenParen,
            "it should be impossible to reach this error lol",
        )?;
        let args = self.argument_list()?;
        self.expect(Token::CloseParen, "Function argument list closure")?;

        Some(Expr::FnCall(ident, args))
    }

    /// this is responsible for parsing the argument list of a function call.
    fn argument_list(&mut self) -> Option<Vec<Expr>> {
        let mut args = Vec::<Expr>::new();
        while self.lexemes.peek().is_some() && !self.next_is(Token::CloseParen) {
            let expr = self.expression()?;
            args.push(expr);
            if self.next_is(Token::Comma) {
                self.lexemes.next();
            }
        }
        Some(args)
    }
}
