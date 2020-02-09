use crate::{
    ast::{ Program, Statement },
    bytecode::Instruction,
    error::CompileError,
};

type Result<T> = std::result::Result<T, CompileError>;

pub struct Compiler {
    ast: Program,
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        Self { ast: program }
    }

    pub fn compile(&self) -> Result<Vec<Instruction>> {
        let main_proc = self.get_main_proc()?;
        todo!()
    }

    fn get_main_proc(&self) -> Result<&Statement> {
        let mut main_proc = None;

        for stmt in &self.ast {
            match stmt {
                Statement::Procedure { ident, .. } => {
                    if ident == "main" {
                        main_proc = Some(stmt);
                    }
                }
                _ => {}
            }
        }

        if let Some(main_proc) = main_proc {
            Ok(main_proc)
        } else {
            Err(CompileError::NoMainFound)
        }
    }
}
