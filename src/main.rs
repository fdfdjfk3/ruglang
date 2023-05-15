mod ast;
mod lexer;
mod navigator;

use std::fs::File;
use std::io::Read;

fn main() {
    println!("Hello, world!");
    let path = std::path::Path::new("test.rgl");
    let mut file = File::open(&path).unwrap();

    let mut text = String::new();
    file.read_to_string(&mut text);

    let tokens = lexer::tokenize(&text);
    for token in &tokens {
        println!("{:?}", token);
    }
    let ast = ast::create_ast(tokens);
    if let ast::AstNode::Program {
        contents: ref c, ..
    } = *ast
    {
        for elem in c {
            println!("{:?}", elem);
        }
    }

    //let a = toker::tokenize(file);
}
