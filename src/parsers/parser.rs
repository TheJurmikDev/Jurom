use crate::{Expression, Statement, Token};

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, position: 0 }
    }

    fn current_token(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or(&Token::EOF)
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    pub(crate) fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        while !matches!(self.current_token(), Token::EOF) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }

        statements
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token() {
            Token::Public => {
                self.advance();
                if matches!(self.current_token(), Token::Class) {
                    self.parse_class()
                } else {
                    None
                }
            }
            Token::Function => self.parse_function(),
            Token::Num => self.parse_variable_declaration(),
            Token::String => self.parse_string_variable_declaration(),
            Token::Identifier(_) => self.parse_assignment_or_method_call(),
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_statement(),
            _ => {
                self.advance();
                None
            }
        }
    }

    fn parse_class(&mut self) -> Option<Statement> {
        self.advance();

        if let Token::Identifier(name) = self.current_token() {
            let class_name = name.clone();
            self.advance();

            if matches!(self.current_token(), Token::LeftBrace) {
                self.advance();

                let mut methods = Vec::new();
                while !matches!(self.current_token(), Token::RightBrace) && !matches!(self.current_token(), Token::EOF) {
                    if let Some(method) = self.parse_statement() {
                        methods.push(method);
                    }
                }

                if matches!(self.current_token(), Token::RightBrace) {
                    self.advance();
                }

                return Some(Statement::ClassDeclaration {
                    name: class_name,
                    methods,
                });
            }
        }

        None
    }

    fn parse_function(&mut self) -> Option<Statement> {
        self.advance();

        if let Token::Identifier(name) = self.current_token() {
            let func_name = name.clone();
            self.advance();

            if matches!(self.current_token(), Token::LeftParen) {
                self.advance();

                while !matches!(self.current_token(), Token::RightParen) && !matches!(self.current_token(), Token::EOF) {
                    self.advance();
                }

                if matches!(self.current_token(), Token::RightParen) {
                    self.advance();
                }

                if matches!(self.current_token(), Token::LeftBrace) {
                    self.advance();

                    let mut body = Vec::new();
                    while !matches!(self.current_token(), Token::RightBrace) && !matches!(self.current_token(), Token::EOF) {
                        if let Some(stmt) = self.parse_statement() {
                            body.push(stmt);
                        }
                    }

                    if matches!(self.current_token(), Token::RightBrace) {
                        self.advance();
                    }

                    return Some(Statement::FunctionDeclaration {
                        name: func_name,
                        body,
                    });
                }
            }
        }

        None
    }

    fn parse_variable_declaration(&mut self) -> Option<Statement> {
        self.advance();

        if let Token::Identifier(name) = self.current_token() {
            let var_name = name.clone();
            self.advance();

            if matches!(self.current_token(), Token::Equals) {
                self.advance();

                if let Some(value) = self.parse_expression() {
                    if matches!(self.current_token(), Token::Semicolon) {
                        self.advance();
                    }

                    return Some(Statement::VariableDeclaration {
                        name: var_name,
                        value,
                    });
                }
            }
        }

        None
    }
    
    fn parse_string_variable_declaration(&mut self) -> Option<Statement> {
        self.advance();

        if let Token::Identifier(name) = self.current_token() {
            let var_name = name.clone();
            self.advance();

            if matches!(self.current_token(), Token::Equals) {
                self.advance();

                if let Some(value) = self.parse_expression() {
                    if matches!(self.current_token(), Token::Semicolon) {
                        self.advance();
                    }

                    return Some(Statement::VariableDeclaration {
                        name: var_name,
                        value,
                    });
                }
            }
        }

        None
    }

    fn parse_assignment_or_method_call(&mut self) -> Option<Statement> {
        if let Token::Identifier(name) = self.current_token() {
            let identifier = name.clone();
            self.advance();

            match self.current_token() {
                Token::Equals => {
                    self.advance();
                    if let Some(value) = self.parse_expression() {
                        if matches!(self.current_token(), Token::Semicolon) {
                            self.advance();
                        }
                        return Some(Statement::Assignment {
                            name: identifier,
                            value,
                        });
                    }
                }
                Token::LeftParen => {
                    self.advance();

                    let mut args = Vec::new();
                    while !matches!(self.current_token(), Token::RightParen) && !matches!(self.current_token(), Token::EOF) {
                        if let Some(arg) = self.parse_expression() {
                            args.push(arg);
                        }
                        if matches!(self.current_token(), Token::RightParen) {
                            break;
                        }
                        self.advance();
                    }

                    if matches!(self.current_token(), Token::RightParen) {
                        self.advance();
                    }

                    if matches!(self.current_token(), Token::Semicolon) {
                        self.advance();
                    }

                    return Some(Statement::MethodCall {
                        object: "".to_string(),
                        method: identifier,
                        args,
                    });
                }
                Token::Dot => {
                    self.advance();
                    if let Token::Identifier(method) = self.current_token() {
                        let method_name = method.clone();
                        self.advance();

                        if matches!(self.current_token(), Token::Dot) {
                            self.advance();
                            if let Token::Identifier(final_method) = self.current_token() {
                                let final_method_name = final_method.clone();
                                self.advance();

                                if matches!(self.current_token(), Token::LeftParen) {
                                    self.advance();

                                    let mut args = Vec::new();
                                    while !matches!(self.current_token(), Token::RightParen) && !matches!(self.current_token(), Token::EOF) {
                                        if let Some(arg) = self.parse_expression() {
                                            args.push(arg);
                                        }
                                        if matches!(self.current_token(), Token::RightParen) {
                                            break;
                                        }
                                        self.advance();
                                    }

                                    if matches!(self.current_token(), Token::RightParen) {
                                        self.advance();
                                    }

                                    if matches!(self.current_token(), Token::Semicolon) {
                                        self.advance();
                                    }

                                    return Some(Statement::MethodCall {
                                        object: format!("{}.{}", identifier, method_name),
                                        method: final_method_name,
                                        args,
                                    });
                                }
                            }
                        } else if matches!(self.current_token(), Token::LeftParen) {
                            self.advance();

                            let mut args = Vec::new();
                            while !matches!(self.current_token(), Token::RightParen) && !matches!(self.current_token(), Token::EOF) {
                                if let Some(arg) = self.parse_expression() {
                                    args.push(arg);
                                }
                                if matches!(self.current_token(), Token::RightParen) {
                                    break;
                                }
                                self.advance();
                            }

                            if matches!(self.current_token(), Token::RightParen) {
                                self.advance();
                            }

                            if matches!(self.current_token(), Token::Semicolon) {
                                self.advance();
                            }

                            return Some(Statement::MethodCall {
                                object: identifier,
                                method: method_name,
                                args,
                            });
                        }
                    }
                }
                _ => {}
            }
        }

        None
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        self.advance();

        if matches!(self.current_token(), Token::LeftParen) {
            self.advance();

            if let Some(condition) = self.parse_expression() {
                if matches!(self.current_token(), Token::RightParen) {
                    self.advance();

                    if matches!(self.current_token(), Token::LeftBrace) {
                        self.advance();

                        let mut then_body = Vec::new();
                        while !matches!(self.current_token(), Token::RightBrace) && !matches!(self.current_token(), Token::EOF) {
                            if let Some(stmt) = self.parse_statement() {
                                then_body.push(stmt);
                            }
                        }

                        if matches!(self.current_token(), Token::RightBrace) {
                            self.advance();
                        }

                        let mut else_body = None;
                        if matches!(self.current_token(), Token::Else) {
                            self.advance();

                            if matches!(self.current_token(), Token::LeftBrace) {
                                self.advance();

                                let mut else_statements = Vec::new();
                                while !matches!(self.current_token(), Token::RightBrace) && !matches!(self.current_token(), Token::EOF) {
                                    if let Some(stmt) = self.parse_statement() {
                                        else_statements.push(stmt);
                                    }
                                }

                                if matches!(self.current_token(), Token::RightBrace) {
                                    self.advance();
                                }

                                else_body = Some(else_statements);
                            }
                        }

                        return Some(Statement::IfStatement {
                            condition,
                            then_body,
                            else_body,
                        });
                    }
                }
            }
        }

        None
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        self.advance();

        if matches!(self.current_token(), Token::LeftParen) {
            self.advance();

            if let Some(condition) = self.parse_expression() {
                if matches!(self.current_token(), Token::RightParen) {
                    self.advance();

                    if matches!(self.current_token(), Token::LeftBrace) {
                        self.advance();

                        let mut body = Vec::new();
                        while !matches!(self.current_token(), Token::RightBrace) && !matches!(self.current_token(), Token::EOF) {
                            if let Some(stmt) = self.parse_statement() {
                                body.push(stmt);
                            }
                        }

                        if matches!(self.current_token(), Token::RightBrace) {
                            self.advance();
                        }

                        return Some(Statement::WhileStatement { condition, body });
                    }
                }
            }
        }

        None
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_logical_or()
    }
    
    fn parse_logical_or(&mut self) -> Option<Expression> {
        let mut left = self.parse_logical_and()?;

        while matches!(self.current_token(), Token::LogicalOr) {
            let operator = "||".to_string();
            self.advance();

            if let Some(right) = self.parse_logical_and() {
                left = Expression::BinaryOp {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Some(left)
    }

    fn parse_logical_and(&mut self) -> Option<Expression> {
        let mut left = self.parse_comparison()?;

        while matches!(self.current_token(), Token::LogicalAnd) {
            let operator = "&&".to_string();
            self.advance();

            if let Some(right) = self.parse_comparison() {
                left = Expression::BinaryOp {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Some(left)
    }

    fn parse_comparison(&mut self) -> Option<Expression> {
        let mut left = self.parse_addition()?;

        while matches!(self.current_token(), 
            Token::DoubleEquals | Token::NotEquals | Token::LessThan | Token::GreaterThan |
            Token::LessThanOrEqual | Token::GreaterThanOrEqual) {

            let operator = match self.current_token() {
                Token::DoubleEquals => "==".to_string(),
                Token::NotEquals => "!=".to_string(),
                Token::LessThan => "<".to_string(),
                Token::GreaterThan => ">".to_string(),
                Token::LessThanOrEqual => "<=".to_string(),
                Token::GreaterThanOrEqual => ">=".to_string(),
                _ => break,
            };
            self.advance();

            if let Some(right) = self.parse_addition() {
                left = Expression::BinaryOp {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Some(left)
    }

    fn parse_addition(&mut self) -> Option<Expression> {
        let mut left = self.parse_multiplication()?;

        while matches!(self.current_token(), Token::Plus | Token::Minus) {
            let operator = match self.current_token() {
                Token::Plus => "+".to_string(),
                Token::Minus => "-".to_string(),
                _ => break,
            };
            self.advance();

            if let Some(right) = self.parse_multiplication() {
                left = Expression::BinaryOp {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Some(left)
    }

    fn parse_multiplication(&mut self) -> Option<Expression> {
        let mut left = self.parse_primary()?;

        while matches!(self.current_token(), Token::Multiply | Token::Divide) {
            let operator = match self.current_token() {
                Token::Multiply => "*".to_string(),
                Token::Divide => "/".to_string(),
                _ => break,
            };
            self.advance();

            if let Some(right) = self.parse_primary() {
                left = Expression::BinaryOp {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Some(left)
    }

    fn parse_primary(&mut self) -> Option<Expression> {
        match self.current_token() {
            Token::Number(n) => {
                let num = *n;
                self.advance();
                Some(Expression::Number(num))
            }
            Token::StringLiteral(s) => {
                let string = s.clone();
                self.advance();
                Some(Expression::String(string))
            }
            Token::Identifier(name) => {
                let var_name = name.clone();
                self.advance();
                Some(Expression::Variable(var_name))
            }
            _ => None,
        }
    }
}