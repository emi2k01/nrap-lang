use crate::token::{Kind, Span, Token};
use crate::error::LexicalError;

pub struct Lexer {
    input: Vec<char>,
    current_char: Option<char>,
    // current_char position in the input
    position: usize,
    line: usize,
    column: usize,
    eof: bool,
}

// TODO: Handle lexical errors
impl Lexer {
    pub fn new(input: &str) -> Self {
        let input: Vec<char> = input.chars().collect();
        let mut lexer = Self {
            input,
            current_char: None,
            position: 0,
            line: 1,
            column: 1,
            eof: false,
        };
        lexer.current_char = lexer.input.get(0).cloned();
        lexer
    }

    pub fn next_token(&mut self) -> Option<Token> {
        // skip whitespace and comments
        loop {
            if self.skip_whitespace() || self.skip_comment() {
                continue;
            }
            break;
        }

        // handle EOF. Only return EOF the first time the lexer has
        // ended lexing all chars. After that return None
        let current_char = match self.current_char {
            Some(v) => v,
            None => {
                if !self.eof {
                    self.eof = true;
                    return Some(Token::new(Kind::EOF, Span::new(self.line, self.column)));
                }
                return None;
            }
        };

        let start_column = self.column;
        use Kind::*;
        let next_token_kind = {
            match current_char {
                '+' => Plus,
                '-' => Minus,
                '*' => Asterisk,
                '/' => Slash,
                '^' => Caret,
                '%' => Modulo,
                '=' => Equal,
                '(' => LParen,
                ')' => RParen,
                '{' => LBrace,
                '}' => RBrace,
                '[' => LBracket,
                ']' => RBracket,
                ',' => Comma,
                ';' => Semicolon,
                '>' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        GreaterThanEqual
                    } else {
                        GreaterThan
                    }
                }
                '<' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        LessThanEqual
                    } else {
                        LessThan
                    }
                }
                '!' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        NotEqual
                    } else {
                        Bang
                    }
                }
                '&' => {
                    if self.peek_char() == Some('&') {
                        self.read_char();
                        And
                    } else {
                        Illegal
                    }
                }
                '|' => {
                    if self.peek_char() == Some('|') {
                        self.read_char();
                        Or
                    } else {
                        Illegal
                    }
                }
                'a'..='z' | 'A'..='Z' | '$' | '_' => self.lex_word(),
                '0'..='9' | '.' => self.lex_number(),
                '"' => self.lex_string(),
                _ => Kind::Illegal,
            }
        };
        self.read_char();
        Some(Token::new(
            next_token_kind,
            Span::new(self.line, start_column),
        ))
    }

    fn lex_word(&mut self) -> Kind {
        // TODO: Handle the dollar sign here instead of relying in the next_token
        // function, so we can use the lex_word function by itself
        let mut word = String::new();

        // This function is only called if there's a valid self.current_char
        // so an error should not happen here
        let current_char = self
            .current_char
            .expect("Error in keyword lexer. This should never happen");
        word.push(current_char.to_owned());

        while let Some(pc) = self.peek_char() {
            if pc.is_ascii_whitespace() || (!pc.is_ascii_alphanumeric() && pc != '_') {
                break;
            }

            let c = self.read_char().unwrap();
            word.push(c);
        }

        match word.as_str() {
            "true" => Kind::BoolLit(true),
            "false" => Kind::BoolLit(false),
            "procedure" => Kind::Procedure,
            "break" => Kind::Break,
            "if" => Kind::If,
            "else" => Kind::Else,
            "loop" => Kind::Loop,
            "in" => Kind::In,
            "out" => Kind::Out,
            _ => Kind::Ident(word),
        }
    }

    fn lex_number(&mut self) -> Kind {
        let mut num_str = String::new();
        num_str.push(self.current_char.unwrap());

        // if the current char is a '.' then it's a decimal number
        let mut is_decimal = self.current_char.map(|c| c == '.').unwrap_or(false);

        while let Some(pc) = self.peek_char() {
            if !pc.is_ascii_digit() && pc != '.' {
                break;
            }

            let c = self.read_char().unwrap();

            if c.is_ascii_digit() {
                num_str.push(c);
            } else if c == '.' {
                // if it already got a point then return error
                if is_decimal {
                    todo!("Return error: already got a decimal point");
                }
                num_str.push(c);
            }
        }

        let number = num_str
            .parse::<f64>()
            .expect("Error in number lexer. This should never happen");
        Kind::FloatLit(number)
    }

    fn lex_string(&mut self) -> Kind {
        let mut escaping = false;
        let mut string = String::new();
        while let Some(c) = self.peek_char() {
            if escaping {
                string.push(match c {
                    'n' => '\n',
                    c => c,
                });
                escaping = false;
                self.read_char();
            } else if c == '\\' {
                escaping = true;
                self.read_char();
            } else if c == '"' {
                self.read_char();
                break;
            } else {
                string.push(c.to_owned());
                self.read_char();
                escaping = false;
            }
        }
        Kind::StringLit(string)
    }

    fn skip_whitespace(&mut self) -> bool {
        let mut got_whitespace = false;
        while let Some(current_char) = self.current_char {
            match current_char {
                ' ' | '\t' => {
                    got_whitespace = true;
                    self.read_char();
                }
                '\n' | '\r' => {
                    got_whitespace = true;
                    self.read_char();
                    self.line += 1;
                    self.column = 1;
                }
                _ => break,
            }
        }
        got_whitespace
    }

    fn skip_comment(&mut self) -> bool {
        let mut got_comment = false;
        while let Some(current_char) = self.current_char {
            match current_char {
                '/' => {
                    if self.peek_char() == Some('/') {
                        got_comment = true;
                        self.read_char();
                        loop {
                            if self.current_char == Some('\n') {
                                break;
                            }
                            self.read_char();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        got_comment
    }

    fn read_char(&mut self) -> Option<char> {
        self.position += 1;
        self.column += 1;
        self.current_char = self.input.get(self.position).cloned();
        self.current_char
    }

    fn peek_char(&mut self) -> Option<char> {
        self.input.get(self.position + 1).cloned()
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    macro_rules! _str {
        ($string:expr) => {
            String::from($string)
        };
    }

    #[test]
    fn test_punctuation_operators() {
        let input = "+-*/=,;==!(){}[]!=<>+<=+>=";
        let lexer = Lexer::new(&input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::Plus, Span::new(1, 1)),
            Token::new(Kind::Minus, Span::new(1, 2)),
            Token::new(Kind::Asterisk, Span::new(1, 3)),
            Token::new(Kind::Slash, Span::new(1, 4)),
            Token::new(Kind::Equal, Span::new(1, 5)),
            Token::new(Kind::Comma, Span::new(1, 6)),
            Token::new(Kind::Semicolon, Span::new(1, 7)),
            Token::new(Kind::Equal, Span::new(1, 8)),
            Token::new(Kind::Equal, Span::new(1, 9)),
            Token::new(Kind::Bang, Span::new(1, 10)),
            Token::new(Kind::LParen, Span::new(1, 11)),
            Token::new(Kind::RParen, Span::new(1, 12)),
            Token::new(Kind::LBrace, Span::new(1, 13)),
            Token::new(Kind::RBrace, Span::new(1, 14)),
            Token::new(Kind::LBracket, Span::new(1, 15)),
            Token::new(Kind::RBracket, Span::new(1, 16)),
            Token::new(Kind::NotEqual, Span::new(1, 17)),
            Token::new(Kind::LessThan, Span::new(1, 19)),
            Token::new(Kind::GreaterThan, Span::new(1, 20)),
            Token::new(Kind::Plus, Span::new(1, 21)),
            Token::new(Kind::LessThanEqual, Span::new(1, 22)),
            Token::new(Kind::Plus, Span::new(1, 24)),
            Token::new(Kind::GreaterThanEqual, Span::new(1, 25)),
            Token::new(Kind::EOF, Span::new(1, 27)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_numbers() {
        let input = ".1 0.2 14 24. 123.456 .21";
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::FloatLit(0.1), Span::new(1, 1)),
            Token::new(Kind::FloatLit(0.2), Span::new(1, 4)),
            Token::new(Kind::FloatLit(14.0), Span::new(1, 8)),
            Token::new(Kind::FloatLit(24.0), Span::new(1, 11)),
            Token::new(Kind::FloatLit(123.456), Span::new(1, 15)),
            Token::new(Kind::FloatLit(0.21), Span::new(1, 23)),
            Token::new(Kind::EOF, Span::new(1, 26)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_keywords() {
        let input = "procedure if else true false loop break variable";
        let lexer = Lexer::new(&input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::Procedure, Span::new(1, 1)),
            Token::new(Kind::If, Span::new(1, 11)),
            Token::new(Kind::Else, Span::new(1, 14)),
            Token::new(Kind::BoolLit(true), Span::new(1, 19)),
            Token::new(Kind::BoolLit(false), Span::new(1, 24)),
            Token::new(Kind::Loop, Span::new(1, 30)),
            Token::new(Kind::Break, Span::new(1, 35)),
            Token::new(Kind::Ident(String::from("variable")), Span::new(1, 41)),
            Token::new(Kind::EOF, Span::new(1, 49)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_bool() {
        let input = "false true";
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::BoolLit(false), Span::new(1, 1)),
            Token::new(Kind::BoolLit(true), Span::new(1, 7)),
            Token::new(Kind::EOF, Span::new(1, 11)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_string() {
        let input = r#" "applé" "®egistered" "\"title\"" "#;
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        // using two-byte wide characters
        let expected_result = vec![
            Token::new(Kind::StringLit(_str!("applé")), Span::new(1, 2)),
            Token::new(Kind::StringLit(_str!("®egistered")), Span::new(1, 10)),
            Token::new(Kind::StringLit(_str!(r#""title""#)), Span::new(1, 23)),
            Token::new(Kind::EOF, Span::new(1, 35)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_program() {
        let input = r#"
            procedure main() {
                x = a+b;
                y = c/d;
                z = x+y;
                x = "string";
                empty = "";
                x = -1. + 2.25;
                v[0] = 1;
                loop {
                    break if(x == b);
                }
                if (x > y) {
                } else if (x == y) {
                } else {
                    if (true == false) {
                    }
                }
            }
        "#;
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::Procedure, Span::new(2, 13)),
            Token::new(Kind::Ident(_str!("main")), Span::new(2, 23)),
            Token::new(Kind::LParen, Span::new(2, 27)),
            Token::new(Kind::RParen, Span::new(2, 28)),
            Token::new(Kind::LBrace, Span::new(2, 30)),
            Token::new(Kind::Ident(_str!("x")), Span::new(3, 17)),
            Token::new(Kind::Equal, Span::new(3, 19)),
            Token::new(Kind::Ident(_str!("a")), Span::new(3, 21)),
            Token::new(Kind::Plus, Span::new(3, 22)),
            Token::new(Kind::Ident(_str!("b")), Span::new(3, 23)),
            Token::new(Kind::Semicolon, Span::new(3, 24)),
            Token::new(Kind::Ident(_str!("y")), Span::new(4, 17)),
            Token::new(Kind::Equal, Span::new(4, 19)),
            Token::new(Kind::Ident(_str!("c")), Span::new(4, 21)),
            Token::new(Kind::Slash, Span::new(4, 22)),
            Token::new(Kind::Ident(_str!("d")), Span::new(4, 23)),
            Token::new(Kind::Semicolon, Span::new(4, 24)),
            Token::new(Kind::Ident(_str!("z")), Span::new(5, 17)),
            Token::new(Kind::Equal, Span::new(5, 19)),
            Token::new(Kind::Ident(_str!("x")), Span::new(5, 21)),
            Token::new(Kind::Plus, Span::new(5, 22)),
            Token::new(Kind::Ident(_str!("y")), Span::new(5, 23)),
            Token::new(Kind::Semicolon, Span::new(5, 24)),
            Token::new(Kind::Ident(_str!("x")), Span::new(6, 17)),
            Token::new(Kind::Equal, Span::new(6, 19)),
            Token::new(Kind::StringLit(_str!("string")), Span::new(6, 21)),
            Token::new(Kind::Semicolon, Span::new(6, 29)),
            Token::new(Kind::Ident(_str!("empty")), Span::new(7, 17)),
            Token::new(Kind::Equal, Span::new(7, 23)),
            Token::new(Kind::StringLit(_str!("")), Span::new(7, 25)),
            Token::new(Kind::Semicolon, Span::new(7, 27)),
            Token::new(Kind::Ident(_str!("x")), Span::new(8, 17)),
            Token::new(Kind::Equal, Span::new(8, 19)),
            Token::new(Kind::Minus, Span::new(8, 21)),
            Token::new(Kind::FloatLit(1.0), Span::new(8, 22)),
            Token::new(Kind::Plus, Span::new(8, 25)),
            Token::new(Kind::FloatLit(2.25), Span::new(8, 27)),
            Token::new(Kind::Semicolon, Span::new(8, 31)),
            Token::new(Kind::Ident(_str!("v")), Span::new(9, 17)),
            Token::new(Kind::LBracket, Span::new(9, 18)),
            Token::new(Kind::FloatLit(0.0), Span::new(9, 19)),
            Token::new(Kind::RBracket, Span::new(9, 20)),
            Token::new(Kind::Equal, Span::new(9, 22)),
            Token::new(Kind::FloatLit(1.0), Span::new(9, 24)),
            Token::new(Kind::Semicolon, Span::new(9, 25)),
            Token::new(Kind::Loop, Span::new(10, 17)),
            Token::new(Kind::LBrace, Span::new(10, 22)),
            Token::new(Kind::Break, Span::new(11, 21)),
            Token::new(Kind::If, Span::new(11, 27)),
            Token::new(Kind::LParen, Span::new(11, 29)),
            Token::new(Kind::Ident(_str!("x")), Span::new(11, 30)),
            Token::new(Kind::Equal, Span::new(11, 32)),
            Token::new(Kind::Equal, Span::new(11, 33)),
            Token::new(Kind::Ident(_str!("b")), Span::new(11, 35)),
            Token::new(Kind::RParen, Span::new(11, 36)),
            Token::new(Kind::Semicolon, Span::new(11, 37)),
            Token::new(Kind::RBrace, Span::new(12, 17)),
            Token::new(Kind::If, Span::new(13, 17)),
            Token::new(Kind::LParen, Span::new(13, 20)),
            Token::new(Kind::Ident(_str!("x")), Span::new(13, 21)),
            Token::new(Kind::GreaterThan, Span::new(13, 23)),
            Token::new(Kind::Ident(_str!("y")), Span::new(13, 25)),
            Token::new(Kind::RParen, Span::new(13, 26)),
            Token::new(Kind::LBrace, Span::new(13, 28)),
            Token::new(Kind::RBrace, Span::new(14, 17)),
            Token::new(Kind::Else, Span::new(14, 19)),
            Token::new(Kind::If, Span::new(14, 24)),
            Token::new(Kind::LParen, Span::new(14, 27)),
            Token::new(Kind::Ident(_str!("x")), Span::new(14, 28)),
            Token::new(Kind::Equal, Span::new(14, 30)),
            Token::new(Kind::Equal, Span::new(14, 31)),
            Token::new(Kind::Ident(_str!("y")), Span::new(14, 33)),
            Token::new(Kind::RParen, Span::new(14, 34)),
            Token::new(Kind::LBrace, Span::new(14, 36)),
            Token::new(Kind::RBrace, Span::new(15, 17)),
            Token::new(Kind::Else, Span::new(15, 19)),
            Token::new(Kind::LBrace, Span::new(15, 24)),
            Token::new(Kind::If, Span::new(16, 21)),
            Token::new(Kind::LParen, Span::new(16, 24)),
            Token::new(Kind::BoolLit(true), Span::new(16, 25)),
            Token::new(Kind::Equal, Span::new(16, 30)),
            Token::new(Kind::Equal, Span::new(16, 31)),
            Token::new(Kind::BoolLit(false), Span::new(16, 33)),
            Token::new(Kind::RParen, Span::new(16, 38)),
            Token::new(Kind::LBrace, Span::new(16, 40)),
            Token::new(Kind::RBrace, Span::new(17, 21)),
            Token::new(Kind::RBrace, Span::new(18, 17)),
            Token::new(Kind::RBrace, Span::new(19, 13)),
            Token::new(Kind::EOF, Span::new(20, 9)),
        ];
        assert_eq!(result, expected_result);
    }
}
