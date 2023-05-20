use std::str::Chars;

pub const EOF: char = '\0';
pub const NEWLINE: char = '\n';

/// An iterator over chars, with a couple of extra functions for convenience.
pub struct Navigator<'a> {
    file_text: &'a str,
    chars: Chars<'a>,
    position: isize,
}

impl<'a> Navigator<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            file_text: input,
            chars: input.chars(),
            position: -1,
        }
    }
    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }
    pub fn second(&self) -> char {
        let mut x = self.chars.clone();
        x.next();
        x.next().unwrap_or(EOF)
    }
    pub fn bump(&mut self) -> char {
        self.position += 1;
        self.chars.next().unwrap_or(EOF)
    }
    pub fn position(&self) -> isize {
        self.position
    }
    pub fn yoink_to_string(&self, start: usize, end: usize) -> String {
        self.file_text[start..end].to_string()
    }
}
