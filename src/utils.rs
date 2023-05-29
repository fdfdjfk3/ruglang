use crate::ast::{AnyOp, BinOp};
use crate::lexer::Token;

pub fn token_to_binop(token: Token) -> BinOp {
    let op = match token {
            Token::Plus => BinOp::Add,
            Token::Minus => BinOp::Sub,
            Token::Slash => BinOp::Div,
            Token::Asterisk => BinOp::Mul,
            _ => panic!("unsupported operation. sorry for the cryptic error but i just want to get this working so i don't really feel like explaining this error any further or attempting to recover from it"),
        };
    op
}

pub fn is_prim_literal(token: Token) -> bool {
    matches!(
        token,
        Token::StrLiteral | Token::IntLiteral | Token::FloatLiteral | Token::BoolLiteral
    )
}

/// not all tokens yet
pub fn is_binary_op(token: Token) -> bool {
    matches!(
        token,
        Token::Plus | Token::Minus | Token::Slash | Token::Asterisk
    )
}

/// comparisons (for ones that return boolean value)
/*
pub fn is_probably_comp_op(token: Token) -> bool {
    matches!(token, Token::Set | Token::
}
*/

pub fn infix_binding_power(op: AnyOp) -> (u8, u8) {
    if let AnyOp::BinOp(op) = op {
        return match op {
            BinOp::Add | BinOp::Sub => (1, 2),
            BinOp::Mul | BinOp::Div => (3, 4),
        };
    }
    (0, 0)
}
