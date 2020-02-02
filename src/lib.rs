pub type Block = Vec<Statement>;

pub enum Expression {
    Ident(String),
    Number(i64),
    Float(f64),
    Bool(bool),
    Call {
        proc: String,
        args: Vec<Expression>,
    },
    Index {
        ident: String,
        indices: Vec<i64>,
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

pub enum Statement {
    Assignment(Assignment),
    If {
        consequence: Block,
        alternative: Block,
    },
    Loop {
        break_if: Expression,
        body: Block,
    },
    Procedure {
        name: String,
        params: Vec<Parameter>,
        body: Block,
    },
    Expression(Expression),
}

pub enum Assignment {
    Variable(String),
    Index(String, Vec<i32>),
}

pub struct Parameter {
    ident: String,
    is_in: bool,
    is_out: bool,
}

pub enum InfixOp {
    // arithmetic
    Sum,
    Minus,
    Mul,
    Div,
    Pow,
    Mod,

    // logical
    And,
    Or,
    Greater,
    GreaterThan,
    Less,
    LessThan,
    Equal,
    NotEqual,
}

pub enum PrefixOp {
    Not,
    Neg,
    Pos,
}
