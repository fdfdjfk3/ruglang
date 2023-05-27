mod ast;
mod lexer;
mod navigator;
mod utils;

use std::fs::File;
use std::io::Read;

fn main() {
    println!("Hello, world!");
    let path = std::path::Path::new("test.rgl");
    let mut file = File::open(&path).unwrap();

    let mut text = String::new();
    file.read_to_string(&mut text).unwrap();
    let tokens = lexer::tokenize(&text);
    /*
    for token in tokens {
        println!("{:?}", token);
    }
    */
    let mut parser = ast::Parser::new(tokens.peekable(), &text);
    let ast = parser.parse();

    /*
    if let ast::AstNode::Program {
        contents: ref c, ..
    } = *ast
    {
        for elem in c {
            println!("{:?}", elem);
        }
    }
    */

    //let a = toker::tokenize(file);
}
