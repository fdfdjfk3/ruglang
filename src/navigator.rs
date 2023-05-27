use std::str::CharIndices;

pub const EOF: char = '\0';
pub const NEWLINE: char = '\n';

/// An iterator over chars, with a couple of extra functions for convenience.
pub struct Navigator<'a> {
    file_text: &'a str,
    chars: CharIndices<'a>,
    position: usize,
}

impl<'a> Navigator<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            file_text: input,
            chars: input.char_indices(),
            position: 0,
        }
    }
    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or((0, EOF)).1
    }
    pub fn second(&self) -> char {
        let mut x = self.chars.clone();
        x.next();
        x.next().unwrap_or((0, EOF)).1
    }
    pub fn bump(&mut self) -> char {
        let (pos, chr) = self.chars.next().unwrap_or((self.file_text.len() - 1, EOF));
        self.position = pos;
        chr
    }
    pub fn position(&self) -> usize {
        self.position
    }
    pub fn yoink_to_string(&self, start: usize, end: usize) -> String {
        self.file_text[start..end].to_string()
    }
}
