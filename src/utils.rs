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
        Token::Plus | Token::Minus | Token::Slash | Token::Asterisk | Token::Set
    )
}

pub fn is_comparison_op(token: Token) -> bool {
    matches!(
        token,
        Token::Eq
            | Token::Ne
            | Token::LeftAngleBracket
            | Token::RightAngleBracket
            | Token::Ge
            | Token::Le
    )
}

pub fn infix_binding_power(op: AnyOp) -> (u8, u8) {
    if let AnyOp::BinOp(op) = op {
        return match op {
            BinOp::Set => (0, 1),
            BinOp::Add | BinOp::Sub => (5, 6),
            BinOp::Mul | BinOp::Div => (7, 8),
        };
    } else if let AnyOp::CompOp(op) = op {
        return match op {
            CompOp::Eq | CompOp::Ne => (1, 2),
            CompOp::Gt | CompOp::Ge | CompOp::Lt | CompOp::Le => (3, 4),
            CompOp::And | CompOp::Or => panic!("not implemented"),
        };
    }
    panic!("Not an infix operation. Your computer is going to explode in 10 seconds.");
}

pub fn prefix_binding_power(op: Token) -> ((), u8) {
    match op {
        Token::Minus => ((), 10),
        Token::Bang => ((), 10),
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

pub fn binary_op_from_token(op: Token) -> AnyOp {
    match op {
        Token::Plus => AnyOp::BinOp(BinOp::Add),
        Token::Minus => AnyOp::BinOp(BinOp::Sub),
        Token::Slash => AnyOp::BinOp(BinOp::Div),
        Token::Asterisk => AnyOp::BinOp(BinOp::Mul),
        Token::Set => AnyOp::BinOp(BinOp::Set),
        _ => panic!("if you got this error, some kind of weird space time anomaly occured, because in the regular parsing code this should be completely impossible to reach. either that or i just need to devote my time and effort to more meaningful hobbies."),
    }
}

pub fn comparison_op_from_token(op: Token) -> AnyOp {
    match op {
        Token::Eq => AnyOp::CompOp(CompOp::Eq),
        Token::Ne => AnyOp::CompOp(CompOp::Ne),
        Token::LeftAngleBracket => AnyOp::CompOp(CompOp::Lt),
        Token::RightAngleBracket => AnyOp::CompOp(CompOp::Gt),
        Token::Ge => AnyOp::CompOp(CompOp::Ge),
        Token::Le => AnyOp::CompOp(CompOp::Le),
        _ => panic!("idgaf"),
    }
}
