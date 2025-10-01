use std::{iter::Peekable, str::Chars};

use crate::{
    position::*,
    token::{Token, TokenKind},
};

/// A lexical scanner that processes input text character by character.
///
/// The Scanner maintains two positions:
/// - `start`: marks the beginning of the current token
/// - `current`: marks the current position in the source text
///
/// This allows the scanner to accumulate characters for tokens while keeping track
/// of their position in the source text.
pub struct Scanner<'a> {
    start: BytePos,
    current: BytePos,
    source: &'a str,
    it: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    /// Creates a new Scanner from the given input string.
    ///
    /// # Arguments
    /// * `buf` - The source text to scan
    pub fn new(buf: &str) -> Scanner<'_> {
        Scanner {
            current: BytePos::default(),
            start: BytePos::default(),
            source: buf,
            it: buf.chars().peekable(),
        }
    }

    /// Returns a reference to the complete source text.
    pub fn source(&self) -> &str {
        self.source
    }

    /// Returns the start position of the current token.
    pub fn start(&self) -> BytePos {
        self.start
    }

    /// Returns the current position of the scanner.
    pub fn current(&self) -> BytePos {
        self.current
    }

    /// Shifts the start position to the current position.
    ///
    /// This should be called before beginning to scan a new token to mark its
    /// starting position.
    pub fn shift(&mut self) {
        self.start = self.current;
    }

    /// Returns a slice of the source text from the start to the current position.
    ///
    /// This is typically used to extract the text of the current token being scanned.
    pub fn slice(&self) -> &str {
        &self.source[self.start.0..self.current.0]
    }

    /// Advances the scanner to the next character and returns it.
    ///
    /// Updates the current position to account for the consumed character.
    ///
    /// # Returns
    /// * `Some(char)` - The next character in the input
    /// * `None` - If the end of input has been reached
    pub fn next(&mut self) -> Option<char> {
        let next = self.it.next();
        if let Some(c) = next {
            self.current = self.current.shift(c);
        }
        next
    }

    /// Returns a reference to the next character without consuming it.
    ///
    /// # Returns
    /// * `Some(&char)` - Reference to the next character
    /// * `None` - If at the end of input
    pub fn peek(&mut self) -> Option<&char> {
        self.it.peek()
    }

    /// Conditionally consumes the current character.
    ///
    /// # Arguments
    /// * `predicate` - A function that takes a char and returns a boolean
    ///
    /// # Returns
    /// `true` if a character was consumed, `false` otherwise
    ///
    pub fn consume_if(&mut self, predicate: impl Fn(char) -> bool) -> bool {
        if let Some(c) = self.peek() {
            if predicate(*c) {
                self.next();
                return true;
            }
        }

        false
    }

    /// Returns true if the next character matches the predicate.
    ///
    /// # Arguments
    /// * `predicate` - A function that takes a char and returns a boolean
    ///
    /// # Returns
    /// `true` if the next character matches the predicate, `false` otherwise
    pub fn if_next<P>(&mut self, predicate: P) -> bool
    where
        P: Fn(char) -> bool,
    {
        if let Some(c) = self.peek() {
            predicate(*c)
        } else {
            false
        }
    }

    /// Conditionally consumes the current character based on what follows it.
    ///
    /// # Arguments
    /// * `predicate` - A function that takes a char and returns a boolean
    ///
    /// # Returns
    /// `true` if a character was consumed, `false` otherwise
    ///
    pub fn consume_if_next<P>(&mut self, predicate: P) -> bool
    where
        P: Fn(char) -> bool,
    {
        let mut it: Peekable<Chars<'a>> = self.it.clone();

        match it.next() {
            Some(_) => {
                if let Some(c) = it.peek() {
                    if predicate(*c) {
                        self.next().unwrap();
                        return true;
                    }
                }
                return false;
            }
            None => return false,
        }
    }

    /// Consumes characters as long as they match the given predicate.
    ///
    /// # Arguments
    /// * `predicate` - A function that takes a char and returns a boolean
    ///
    /// # Returns
    /// A vector containing all consumed characters
    pub fn consume_while<P>(&mut self, predicate: P) -> Vec<char>
    where
        P: Fn(char) -> bool,
    {
        let mut consumed = Vec::new();
        while let Some(&c) = self.peek() {
            if predicate(c) {
                consumed.push(c);
                self.next().unwrap();
            } else {
                break;
            }
        }
        consumed
    }

    /// Consumes the next character if it matches the expected character.
    ///
    /// # Arguments
    /// * `expected` - The character to match against
    ///
    /// # Returns
    /// `true` if the character matched and was consumed, `false` otherwise
    pub fn next_match(&mut self, expected: char) -> bool {
        if self.peek() == Some(&expected) {
            self.next();
            true
        } else {
            false
        }
    }

    /// Create a token with the current len
    pub fn create_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            len: self.current.0 - self.start.0,
        }
    }

    /// Returns a copy of the iterator over the characters in the source text.
    ///
    /// # Returns
    /// A copy of the iterator over the characters in the source text
    pub fn iterator(&self) -> impl Iterator<Item = char> + use<'a> {
        self.it.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next() {
        let mut scanner = Scanner::new("123🦀€é");
        assert_eq!(scanner.slice(), "");
        scanner.next();
        assert_eq!(scanner.slice(), "1");
        scanner.next();
        assert_eq!(scanner.slice(), "12");
        scanner.next();
        assert_eq!(scanner.slice(), "123");
        scanner.next();
        assert_eq!(scanner.slice(), "123🦀");
        scanner.next();
        assert_eq!(scanner.slice(), "123🦀€");
        scanner.next();
        assert_eq!(scanner.slice(), "123🦀€é");
    }

    #[test]
    fn test_peek() {
        let mut scanner = Scanner::new("123🦀€é");
        assert_eq!(scanner.peek(), Some(&'1'));
        scanner.next();
        assert_eq!(scanner.peek(), Some(&'2'));
        scanner.next();
        assert_eq!(scanner.peek(), Some(&'3'));
        scanner.next();
        assert_eq!(scanner.peek(), Some(&'🦀'));
        scanner.next();
        assert_eq!(scanner.peek(), Some(&'€'));
        scanner.next();
        assert_eq!(scanner.peek(), Some(&'é'));
        scanner.next();
        assert_eq!(scanner.peek(), None);
    }

    #[test]
    fn test_consume_if() {
        let mut scanner = Scanner::new("123abc");
        assert!(scanner.consume_if(|c| c.is_numeric()));
        assert_eq!(scanner.slice(), "1");
        assert!(scanner.consume_if(|c| c.is_numeric()));
        assert_eq!(scanner.slice(), "12");
        assert!(scanner.consume_if(|c| c.is_numeric()));
        assert_eq!(scanner.slice(), "123");
        assert!(!scanner.consume_if(|c| c.is_numeric()));
        assert_eq!(scanner.slice(), "123");
    }

    #[test]
    fn test_consume_if_next() {
        let mut scanner = Scanner::new("123abc");
        assert!(scanner.consume_if_next(|c| c.is_numeric()));
        assert_eq!(scanner.slice(), "1");
        assert!(scanner.consume_if_next(|c| c.is_numeric()));
        assert_eq!(scanner.slice(), "12");
        assert!(!scanner.consume_if_next(|c| c.is_numeric()));
        assert_eq!(scanner.slice(), "12");
    }

    #[test]
    fn test_if_next() {
        let mut scanner = Scanner::new("123abc");
        assert!(scanner.if_next(|c| c.is_numeric()));
        // Does not consume the character
        assert_eq!(scanner.slice(), "");
    }
}
