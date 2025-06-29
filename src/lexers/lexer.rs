use crate::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current_char = if chars.is_empty() { None } else { Some(chars[0]) };

        Lexer {
            input: chars,
            position: 0,
            current_char,
        }
    }

    fn advance(&mut self) {
        self.position += 1;
        if self.position >= self.input.len() {
            self.current_char = None;
        } else {
            self.current_char = Some(self.input[self.position]);
        }
    }

    fn peek(&self) -> Option<char> {
        if self.position + 1 >= self.input.len() {
            None
        } else {
            Some(self.input[self.position + 1])
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_string(&mut self) -> String {
        let mut result = String::new();
        self.advance();

        while let Some(ch) = self.current_char {
            if ch == '"' {
                self.advance();
                break;
            }
            result.push(ch);
            self.advance();
        }

        result
    }

    fn read_number(&mut self) -> i64 {
        let mut result = String::new();
        let mut is_negative = false;
        
        if let Some('-') = self.current_char {
            if let Some(next_ch) = self.peek() {
                if next_ch.is_ascii_digit() {
                    is_negative = true;
                    self.advance();
                }
            }
        }

        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let mut num = result.parse().unwrap_or(0);
        if is_negative {
            num = -num;
        }
        num
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        result
    }
    
    fn should_parse_negative_number(&self) -> bool {
        if let Some(next_ch) = self.peek() {
            if next_ch.is_ascii_digit() {
                if self.position == 0 {
                    return true;
                }

                let mut prev_pos = self.position.saturating_sub(1);
                while prev_pos > 0 {
                    match self.input[prev_pos] {
                        ' ' | '\t' | '\n' | '\r' => {
                            prev_pos = prev_pos.saturating_sub(1);
                            continue;
                        }
                        '=' | '(' | ',' | '{' => return true,
                        _ => return false,
                    }
                }
                return true;
            }
        }
        false
    }

    pub(crate) fn next_token(&mut self) -> Token {
        loop {
            match self.current_char {
                None => return Token::EOF,
                Some(' ') | Some('\t') | Some('\n') | Some('\r') => {
                    self.skip_whitespace();
                    continue;
                }
                Some('{') => {
                    self.advance();
                    return Token::LeftBrace;
                }
                Some('}') => {
                    self.advance();
                    return Token::RightBrace;
                }
                Some('(') => {
                    self.advance();
                    return Token::LeftParen;
                }
                Some(')') => {
                    self.advance();
                    return Token::RightParen;
                }
                Some(';') => {
                    self.advance();
                    return Token::Semicolon;
                }
                Some('.') => {
                    self.advance();
                    return Token::Dot;
                }
                Some('=') => {
                    if self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        return Token::DoubleEquals;
                    }
                    self.advance();
                    return Token::Equals;
                }
                Some('!') => {
                    if self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        return Token::NotEquals;
                    }
                    self.advance();
                    continue;
                }
                Some('<') => {
                    if self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        return Token::LessThanOrEqual;
                    }
                    self.advance();
                    return Token::LessThan;
                }
                Some('>') => {
                    if self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        return Token::GreaterThanOrEqual;
                    }
                    self.advance();
                    return Token::GreaterThan;
                }
                Some('&') => {
                    if self.peek() == Some('&') {
                        self.advance();
                        self.advance();
                        return Token::LogicalAnd;
                    }
                    self.advance();
                    continue;
                }
                Some('|') => {
                    if self.peek() == Some('|') {
                        self.advance();
                        self.advance();
                        return Token::LogicalOr;
                    }
                    self.advance();
                    continue;
                }
                Some('+') => {
                    self.advance();
                    return Token::Plus;
                }
                Some('-') => {
                    if self.should_parse_negative_number() {
                        let num = self.read_number();
                        return Token::Number(num);
                    } else {
                        self.advance();
                        return Token::Minus;
                    }
                }
                Some('*') => {
                    self.advance();
                    return Token::Multiply;
                }
                Some('/') => {
                    self.advance();
                    return Token::Divide;
                }
                Some('"') => {
                    let string_val = self.read_string();
                    return Token::StringLiteral(string_val);
                }
                Some(ch) if ch.is_ascii_digit() => {
                    let num = self.read_number();
                    return Token::Number(num);
                }
                Some(ch) if ch.is_alphabetic() || ch == '_' => {
                    let identifier = self.read_identifier();
                    return match identifier.as_str() {
                        "public" => Token::Public,
                        "class" => Token::Class,
                        "function" => Token::Function,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "num" => Token::Num,
                        "string" => Token::String,
                        _ => Token::Identifier(identifier),
                    };
                }
                Some(_) => {
                    self.advance();
                    continue;
                }
            }
        }
    }
}