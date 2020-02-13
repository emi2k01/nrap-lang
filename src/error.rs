use std::fmt;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexicalError {
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("There's a syntax error in your program")]
    Generic,
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("main procedure not present")]
    NoMainFound,
}

#[derive(Error, Debug, Clone)]
pub enum RuntimeError {
    #[error("There was an error: {0}")]
    Generic(String),

    #[error("Tried to use an invalid expression in «out» argument. «Out» argument must be an identifier")]
    OutParamNoIdent,

    #[error("Tried to use an invalid expression in index operation. Index must be a number")]
    IndexNotANumber,

    #[error("Tried to use a non-procedure statement in the top level of the program. \
            All non-procedure statements must be inside a procedure")]
    NonProcedureInTopLevel,

    #[error("Tried to use an invalid expression in «if» statement. \
            «if» conditions must be bool expressions")]
    IfConditionNotABool,

    #[error("Tried to use an invalid expression in «break if» statement. \
            «break if» conditions must be bool expressions")]
    BreakIfConditionNotABool,

    #[error("Tried to use an invalid procedure in a call expression. To use a procedure in a call \
            statement, it must have only one «out» parameter and it must be the last one")]
    ProcedureCallExprNotValid,

    #[error("Tried to use an infix operation with invalid types.")]
    InfixOpWithInvalidTypes,

    #[error("Tried to use a prefix operation with an invalid type")]
    PrefixOpWithInvalidType,

    #[error("There's no main procedure")]
    NoMainProcedure,
}
