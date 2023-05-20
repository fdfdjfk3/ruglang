use crate::lexer::{Lexeme, Token};
use crate::navigator::{EOF, NEWLINE};
use std::iter::Peekable;

type Ast = Vec<AstNode>;

#[derive(Debug, Clone)]
pub enum AstNode {
    FunctionDecl(FunctionDecl),
    GlobalVarDecl(VarDecl),
    VarDecl(VarDecl),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
}

#[derive(Debug, Clone)]
pub struct Block {
    statements: Vec<AstNode>,
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
    pub fn parse(&mut self) -> Ast {
        self.program()
    }
    fn row_and_col_of(&self, lexeme: &Lexeme) -> (usize, usize) {
        let substr = &self.file_str[0..=lexeme.span.start];

        let row = substr.chars().filter(|ch| *ch == NEWLINE).count();
        let mut col = 0;
        if row != 0 {
            for (i, ch) in substr.chars().rev().enumerate() {
                if ch == '\n' {
                    break;
                }
                col = i;
            }
        } else {
            col = lexeme.span.start;
        }
        (row, col)
    }
    fn error(&mut self, error_string: String, lexeme: &Lexeme) {
        let (row, col) = self.row_and_col_of(lexeme);

        self.errors
            .push(format!("{}:{} > {}", row, col, error_string));
    }
    fn program(&mut self) -> Ast {
        let mut ast: Ast = Vec::<AstNode>::new();

        while self.lexemes.peek().is_some() {
            match self.lexemes.next().unwrap().token {
                Token::Function => ast.push(self.function_declaration()),
                Token::Var => ast.push(self.global_variable_declaration()),
                _ => unimplemented!(),
            }
        }
        ast
    }
    fn function_declaration(&mut self) -> AstNode {
        let ident = self.ident();
        if self.lexemes.peek().is_none() {
            panic!("missing opening bracket on function declaration");
        }
        if self.lexemes.next().unwrap().token != Token::OpenParen {
            panic!("missing opening bracket or malformed argument list in function declaration");
        }
        let mut params: Vec<(String, Type)> = Vec::new();
        while self.lexemes.peek().unwrap().token == Token::Ident {
            let param = self.ident_with_explicit_type();
            params.push(param);
        }
        unimplemented!()
        /*
        FunctionDecl(FunctionDecl { identifier: ident, params, /*returntype, block*/}
        */
    }
    fn ident_with_explicit_type(&mut self) -> (String, Type) {
        let ident = self.ident();
        let vartype = self.explicit_type();
        (ident, vartype)
    }
    pub fn explicit_type(&mut self) -> Type {
        if self.lexemes.peek().is_none() || self.lexemes.peek().unwrap().token != Token::As {
            panic!("expected 'as'");
        }
        self.lexemes.next();
        let vartype = self.type_identifier();
        vartype
    }
    fn type_identifier(&mut self) -> Type {
        if self.lexemes.peek().is_none() {
            panic!("expected type aftre 'as'");
        }
        match self.lexemes.peek().unwrap().token {
            Token::IntType => Type::Int,
            Token::FloatType => Type::Float,
            Token::BoolType => Type::Bool,
            Token::StrType => Type::Str,
            Token::NothingType => Type::Nothing,
            _ => panic!("unknown type"),
        }
    }
    fn ident(&mut self) -> String {
        let lexeme = self.lexemes.next().unwrap();
        if lexeme.token == Token::Ident {
            return self.file_str[lexeme.span.start..lexeme.span.end].to_owned();
        } else {
            panic!("expected ident");
        }
    }
    fn global_variable_declaration(&mut self) -> AstNode {
        unimplemented!()
    }
}

// unused vvvvvvvv just keeping it because idk

/*

/// fuck
#[derive(Debug)]
pub enum SubExpression {
    SingleLiteral {
        value: LiteralType,
    },
    SingleIdent {
        name: String,
    },
    MathTerm {
        left: Box<SubExpression>,
        operation: BinaryOp,
        right: Box<SubExpression>,
    },
    ComparisonTerm {
        left: Box<SubExpression>,
        operation: Comparison,
        right: Box<SubExpression>,
    },
}

impl SubExpression {
    pub fn single_thing_from(token: Token) -> Self {
        match token {
            Token::Ident { val } => Self::SingleIdent { name: val },
            Token::Literal { lit_type } => Self::SingleLiteral { value: lit_type },
            _ => panic!("die"),
        }
    }

    pub fn from(expr: &[Token]) -> Self {
        if expr.len() == 1 {
            if expr[0].could_be_a_value() {
                return Self::single_thing_from(expr[0].clone());
            }
        }

        let mut split_at: Option<usize> = None;
        let mut split_precedence: u8 = 0;
        for (i, token) in expr.iter().rev().enumerate() {
            if !token.could_be_a_value() {
                if split_at.is_none() || split_precedence < token.operator_precedence() {
                    println!("new potential lowest: {:?} {}", token, i);
                    split_at = Some(expr.len() - i - 1);
                    split_precedence = token.operator_precedence();
                }
            }
            if split_precedence == 2 {
                break;
            }
        }
        println!("{}", split_precedence);
        if split_at.is_none() {
            panic!("this should never be reached");
        }
        Self::MathTerm {
            left: Box::new(Self::from(&expr[0..split_at.unwrap()])),
            operation: BinaryOp::from_token(expr[split_at.unwrap()].clone()),
            right: Box::new(Self::from(&expr[split_at.unwrap() + 1..expr.len()])),
        }
    }
}

/// a type that contains the actual data
#[derive(Debug)]
pub enum TypedData {
    Integer { value: i64 },
    Float { value: f64 },
    Str { value: String },
    Boolean { value: bool },
    Nothing,
    //Custom { value: Vec<TypedData> },
}

/// just a flag for a type
#[derive(Debug)]
pub enum Type {
    Integer,
    Float,
    Str,
    Boolean,
    Nothing,
    //Custom(String),
}

#[derive(Debug)]
pub enum Comparison {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    pub fn from_token(token: Token) -> Self {
        match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Asterisk => Self::Mul,
            Token::Slash => Self::Div,
            _ => panic!("ugh"),
        }
    }
}

/// why do i even try, why do i invest the time and effort in fruitless endeavours, why do i keep
/// trying even though it's clear i'm not capable. i begin a project like "hooo yeah i'm gonna get
/// this done" and each time it turns out like shit cus i always prematurely give up without
/// expending too much effort actually trying to bring it to a working state. and this program is
/// no exception either.
/// this struct does stuff (shittily)
pub struct IdkWtfImDoing {
    ast: Box<AstNode>,
    tokens: Vec<Token>,
    index: isize,
}

/// create a ruglang AST from the token iterator provided. this will panic if the token stream is
/// not valid.
pub fn create_ast(mut tokens: Vec<Token>) -> Box<AstNode> {
    let mut ast = Box::new(AstNode::Program {
        name: String::from("test.rgl"),
        contents: vec![],
    });

    let mut assemble_ast = IdkWtfImDoing {
        ast,
        tokens,
        index: -1,
    };
    assemble_ast.assemble_ast();
    assemble_ast.ast
}

impl IdkWtfImDoing {
    pub fn bump(&mut self) -> Option<&Token> {
        self.index += 1;
        self.tokens.get(self.index as usize)
    }
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get((self.index + 1) as usize)
    }
    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.index as usize)
    }
    pub fn prev(&self) -> Option<&Token> {
        self.tokens.get(self.index as usize - 1)
    }
    pub fn assemble_ast(&mut self) {
        let mut program: Vec<Box<AstNode>> = Vec::new();
        while self.peek().is_some() {
            if self.peek() == Some(&Token::Semicolon) {
                self.bump();
                continue;
            }
            let thingy = self.next_node();
            if thingy.is_none() {
                panic!("idk what to do here yet lol");
            }
            program.push(thingy.unwrap());
        }
        if let AstNode::Program {
            contents: ref mut c,
            ..
        } = *self.ast
        {
            *c = program;
        }
    }
    pub fn next_node(&mut self) -> Option<Box<AstNode>> {
        match self.peek() {
            Some(Token::Var) => self.expect_var_def(),
            _ => None,
        }
    }
    pub fn expect_var_def(&mut self) -> Option<Box<AstNode>> {
        self.bump();
        let name = self.expect_ident();
        self.expect_equal_sign();
        let value = self.expect_expression();
        Some(Box::new(AstNode::VariableDecl { name, value }))
    }
    /// Expect an identifier to be the next thing. This function can panic because right now
    /// there's no proper error handling.
    pub fn expect_ident(&mut self) -> String {
        let next = self.bump();
        match next {
            Some(Token::Ident { val }) => val.to_string(),
            _ => panic!("there was supposed to be a variable name here but it got fricked up lol"),
        }
    }
    /// Panics if there's no equal sign.
    pub fn expect_equal_sign(&mut self) {
        let next = self.bump();
        match next {
            Some(Token::Equals) => (),
            _ => panic!("no equal sign whree it was supposed to beeeeee"),
        }
    }
    pub fn valid_expression_start(&self, index: usize) -> bool {
        let current = self.tokens.get(index);
        if current.is_none() {
            return false;
        }
        current.unwrap().could_be_a_value()
    }
    pub fn valid_expression_continue(&self, index: usize) -> bool {
        let prev = self.tokens.get(index - 1);
        let current = self.tokens.get(index);
        let next = self.tokens.get(index + 1);
        if current.is_none() || prev.is_none() || next.is_none() {
            return false;
        }
        if current.unwrap().could_be_a_value() {
            return true;
        }
        if current.unwrap().can_be_operator() {
            if prev.unwrap().could_be_a_value() && next.unwrap().could_be_a_value() {
                return true;
            }
        }
        false
    }
    pub fn expect_expression(&mut self) -> Box<AstNode> {
        // find where the expression starts and ends
        if !self.valid_expression_start(self.index as usize + 1) {
            panic!("not an expression, fuck");
        }
        self.bump();
        let start = self.index as usize;

        while self.valid_expression_continue(self.index as usize + 1) {
            self.bump();
        }
        let end = self.index as usize;
        println!("{:?}, {}, {}", &self.tokens[start..=end], start, end);
        if (end - start + 1) == 0 || (end - start + 1) % 2 == 0 {
            panic!("idk what to do here ughhhh");
        }
        // do the thing
        let expr = SubExpression::from(&self.tokens[start..=end]);
        println!("{:?}", expr);
        Box::new(AstNode::Expression { expr })
    }
}
*/
