use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Boolean(bool),
    Number(f64),
    Dot,
    Comma,
    String(Vec<u8>),
    Nil,
    Add,
    Subtract,
    Multiply,
    Divide,
    Semicolon,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    EqualTo,
    NotEqualTo,
    Not,
    And,
    Or,
    OpeningParen,
    ClosingParen,
    OpeningBrace,
    ClosingBrace,
    Var,
    Assignment,
    If,
    Else,
    While,
    For,
    Identifier(Vec<u8>),
    Function,
    Return,
    Class,
    This,
    Super,
    Print,
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    InvalidToken,
    InvalidSourceCharacter,
}

#[derive(Debug, Clone)]
pub struct TokenLocation {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct MetaToken {
    tok: Token,
    loc: TokenLocation, 
}

static KEYWORDS: LazyLock<HashMap<&str, Token>> = LazyLock::new(|| {
    let mut map = HashMap::new();

    map.insert("true", Token::Boolean(true));
    map.insert("false", Token::Boolean(false));
    map.insert("and", Token::And);
    map.insert("var", Token::Var);
    map.insert("or", Token::Or);
    map.insert("print", Token::Print);
    map.insert("else", Token::Else);
    map.insert("while", Token::While);
    map.insert("for", Token::For);
    map.insert("fun", Token::Function);
    map.insert("return", Token::Return);
    map.insert("class", Token::Class);
    map.insert("this", Token::This);
    map.insert("super", Token::Super);
    map.insert("if", Token::If);
    map.insert("nil", Token::Nil);

    map
});

pub type LexResult = Result<Token, LexError>;

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a [u8],
    curr_start: usize,
    curr_end: usize,
    curr_line: usize, // this is used for the sole purpose of error diagnostics
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Result<Self, LexError> {
        if !source.is_ascii() {
            return Err(LexError::InvalidSourceCharacter);
        }

        Ok(Lexer {
            source,
            curr_start: 0,
            curr_end: 0,
            curr_line: 1,
        })
    }

    pub fn next_token(&mut self) -> LexResult {
        let mut next_char = self.peek_curr();
        if let Some(char) = next_char {
            if char == b' ' || char == b'\n' {
                self.skip_whitespace();
                next_char = self.advance();
            }
        }
        self.curr_start = self.curr_end;

        if let Some(c) = next_char {
            let tok: LexResult = match c {
                b'.' => Ok(Token::Dot),
                b';' => Ok(Token::Semicolon),
                b',' => Ok(Token::Comma),
                b'(' => Ok(Token::OpeningParen),
                b')' => Ok(Token::ClosingParen),
                b'{' => Ok(Token::OpeningBrace),
                b'}' => Ok(Token::ClosingBrace),
                b'-' => Ok(Token::Subtract),
                b'+' => Ok(Token::Add),
                b'*' => Ok(Token::Multiply),
                b'/' => Ok(Token::Divide),
                b'>' => self.greater_specify(),
                b'<' => self.lesser_specify(),
                b'=' => self.equal_specify(),
                b'!' => self.exclamation_specify(),
                b'"' => self.string_literal(),
                _ => {
                    if c.is_ascii_digit() {
                        self.number_literal()
                    } else if c.is_ascii_alphabetic() || c == b'_' {
                        self.identifier_or_keyword()
                    } else {
                        Err(LexError::InvalidToken)
                    }
                }
            };

            self.advance();

            tok
        } else {
            Ok(Token::EOF)
        }
    }

    fn number_literal(&mut self) -> LexResult {
        let mut encountered_dot = false;
        let mut num_str: Vec<u8> = vec![self.peek_curr().unwrap()];
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                num_str.push(c);
            } else {
                if c == b'.' {
                    if encountered_dot {
                        return Err(LexError::InvalidToken);
                    }
                    num_str.push(c);
                    encountered_dot = true;
                } else {
                    break;
                }
            }
            self.advance();
        }
        if *num_str.last().unwrap() == b'.' {
            return Err(LexError::InvalidToken);
        }
        Ok(Token::Number(
            String::from_utf8(num_str).unwrap().parse().unwrap(),
        ))
    }

    fn identifier_or_keyword(&mut self) -> LexResult {
        let mut word: Vec<u8> = vec![self.peek_curr().unwrap()];
        while let Some(c) = self.peek() {
            if !c.is_ascii_alphanumeric() && c != b'_' {
                break;
            } else {
                word.push(c);
                self.advance();
            }
        }
        let word_str = String::from_utf8(word.clone()).unwrap();
        if KEYWORDS.contains_key(word_str.as_str()) {
            Ok(KEYWORDS.get(word_str.as_str()).unwrap().clone())
        } else {
            Ok(Token::Identifier(word))
        }
    }

    fn string_literal(&mut self) -> LexResult {
        let mut string = vec![];
        loop {
            let next = self.advance();
            if next.is_some() {
                let char = next.unwrap();
                if char == b'"' {
                    break;
                } else {
                    string.push(char);
                }
            } else {
                return Err(LexError::InvalidToken);
            }
        }
        Ok(Token::String(string))
    }

    fn greater_specify(&mut self) -> LexResult {
        let next_char = self.peek();
        if let Some(next) = next_char
            && next == b'='
        {
            self.advance();
            Ok(Token::GreaterThanOrEqual)
        } else {
            Ok(Token::GreaterThan)
        }
    }

    fn lesser_specify(&mut self) -> LexResult {
        let next_char = self.peek();
        if let Some(next) = next_char
            && next == b'='
        {
            self.advance();
            Ok(Token::LessThanOrEqual)
        } else {
            Ok(Token::LessThan)
        }
    }

    fn equal_specify(&mut self) -> LexResult {
        if let Some(next) = self.peek()
            && next == b'='
        {
            self.advance();
            Ok(Token::EqualTo)
        } else {
            Ok(Token::Assignment)
        }
    }

    fn exclamation_specify(&mut self) -> LexResult {
        if let Some(next) = self.peek()
            && next == b'='
        {
            self.advance();
            Ok(Token::NotEqualTo)
        } else {
            Ok(Token::Not)
        }
    }

    pub fn token_pos(&self) -> TokenLocation {
        TokenLocation {
            line: self.curr_line,
            col: self.curr_start + 1,
        }
    }

    fn advance(&mut self) -> Option<u8> {
        let peek_result = self.peek();
        self.curr_end += 1;
        peek_result
    }

    fn peek(&self) -> Option<u8> {
        if self.curr_end >= self.source.len() - 1 {
            return None;
        }
        Some(self.source[self.curr_end + 1])
    }

    fn peek_curr(&self) -> Option<u8> {
        if self.curr_end >= self.source.len() {
            return None;
        }
        Some(self.source[self.curr_end])
    }

    fn skip_whitespace(&mut self) {
        if self.peek_curr().unwrap() == b'\n' {
            self.curr_line += 1;
        }
        while let Some(c) = self.peek() {
            if c == b' ' {
                self.advance();
            } else if c == b'\n' {
                self.advance();
                self.curr_line += 1;
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn newline() {
        let mut lexer = Lexer::new("fun test()\n(\n\n\n)".as_bytes()).unwrap();
        assert_eq!(lexer.next_token().unwrap(), Token::Function, "1");
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Identifier(vec![b't', b'e', b's', b't']),
            "2"
        );
        assert_eq!(lexer.next_token().unwrap(), Token::ClosingParen, "4");
        assert_eq!(lexer.next_token().unwrap(), Token::OpeningParen, "5");
        assert_eq!(lexer.next_token().unwrap(), Token::ClosingParen, "6");
        assert_eq!(lexer.next_token().unwrap(), Token::EOF, "7");
    }

    #[test]
    fn number_whole() {
        let mut lexer = Lexer::new("53".as_bytes()).unwrap();
        assert_eq!(lexer.next_token().unwrap(), Token::Number(53.0));
    }

    #[test]
    fn number_decimal() {
        let mut lexer = Lexer::new("53.13".as_bytes()).unwrap();
        assert_eq!(lexer.next_token().unwrap(), Token::Number(53.13));
    }

    #[test]
    fn number_invalid() {
        let mut lexer = Lexer::new("53.12.0".as_bytes()).unwrap();
        assert_eq!(lexer.next_token().unwrap_err(), LexError::InvalidToken);
    }
}
