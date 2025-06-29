#[derive(Debug, Clone)]
pub enum Token {
    Public,
    Class,
    Function,
    If,
    Else,
    While,
    Num,
    String,
    Identifier(String),
    Number(i64),
    StringLiteral(String),
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Semicolon,
    Dot,
    Equals,
    DoubleEquals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    Plus,
    Minus,
    Multiply,
    Divide,
    EOF,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ClassDeclaration {
        name: String,
        methods: Vec<Statement>,
    },
    FunctionDeclaration {
        name: String,
        body: Vec<Statement>,
    },
    VariableDeclaration {
        name: String,
        value: Expression,
    },
    Assignment {
        name: String,
        value: Expression,
    },
    MethodCall {
        object: String,
        method: String,
        args: Vec<Expression>,
    },
    IfStatement {
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    WhileStatement {
        condition: Expression,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Number(i64),
    String(String),
    Variable(String),
    BinaryOp {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
}