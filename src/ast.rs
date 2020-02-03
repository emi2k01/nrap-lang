#![allow(unused)]

pub type BlockStatement = Vec<Statement>;
pub type Program = Vec<Statement>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Ident(String),
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Call {
        proc: String,
        args: Vec<Expression>,
    },
    Index {
        ident: String,
        indices: Vec<Expression>,
    },
    Infix {
        op: InfixOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Prefix {
        op: PrefixOp,
        right: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assignment {
        ident: String,
        index: Option<Vec<Expression>>,
        value: Expression,
    },
    If {
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Loop {
        body: BlockStatement,
    },
    Procedure {
        ident: String,
        params: Vec<Parameter>,
        body: BlockStatement,
    },
    BreakIf {
        condition: Expression,
    },
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub ident: String,
    pub is_in: bool,
    pub is_out: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    // arithmetic
    Sum,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,

    // logical
    And,
    Or,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Not,
    Neg,
    Pos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    LogicalOp,
    Equals,
    LessGreater,
    Sum,
    Product,
    Power,
    Prefix,
    Call,
    Index,
}
