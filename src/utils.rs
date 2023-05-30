use crate::ast::{AnyOp, BinOp, CompOp, UnaryOp};
use crate::lexer::Token;

pub fn is_prim_literal(token: Token) -> bool {
    matches!(
        token,
        Token::StrLiteral | Token::IntLiteral | Token::FloatLiteral | Token::BoolLiteral
    )
}

pub fn is_prefix_op(token: Token) -> bool {
    matches!(token, Token::Minus | Token::Bang)
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
    panic!("Not an infix operation. Your computer is going to explode in 10 seconds.");
}

pub fn prefix_binding_power(op: Token) -> ((), u8) {
    match op {
        Token::Minus => ((), 5),
        Token::Bang => ((), 5),
        _ => panic!("Not a prefix operation L bozo"),
    }
}

pub fn prefix_op_from_token(op: Token) -> UnaryOp {
    match op {
        Token::Minus => UnaryOp::Neg,
        Token::Bang => UnaryOp::Not,
        _ => panic!("fdsfjlk d fjsl nvd fiw"),
    }
}
