use super::object::Object;
use crate::ast::{Expression, InfixOp, PrefixOp, Program, Statement};
use crate::error::RuntimeError;

type Result<T> = std::result::Result<T, RuntimeError>;

pub struct Interpreter {
    program: Program,
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self { program }
    }

    pub fn run(&self) -> Result<()> {
        let main_proc = self.main_proc().ok_or(RuntimeError::NoMainProcedure)?;

        for stmt in main_proc {
            self.eval_statement(stmt);
        }

        Ok(())
    }

    fn eval_statement(&self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Expression(expr) => self.eval_expression(expr)?,
            _ => todo!(),
        };
        todo!()
    }

    fn eval_expression(&self, expr: &Expression) -> Result<Object> {
        Ok(match expr {
            Expression::Infix { op, left, right } => {
                let left_obj = self.eval_expression(left)?;
                let right_obj = self.eval_expression(right)?;

                self.eval_infix(op, &left_obj, &right_obj)?
            },
            Expression::Prefix { op, right } => {
                let right_obj = self.eval_expression(right)?;

                self.eval_prefix(op, &right_obj)?
            },
            Expression::Float(f) => Object::Float(*f),
            Expression::String(s) => Object::String(s.to_owned()),
            Expression::Bool(b) => Object::Bool(*b),
            // TODO: handle more expressions
            _ => return Err(RuntimeError::Generic("Todo: Invalid expression".to_string())),
        })
    }

    fn eval_infix(&self, op: &InfixOp, l: &Object, r: &Object) -> Result<Object> {
        match l {
            Object::Bool(lv) => match r {
                Object::Bool(rv) => self.eval_infix_bool(op, *lv, *rv),
                //Object::String(rv) => self.eval_infix_string_concat(op, lv, rv),
                _ => Err(RuntimeError::InfixOpWithInvalidTypes),
            },
            Object::Float(lv) => match r {
                Object::Float(rv) => self.eval_infix_float(op, *lv, *rv),
                //Object::String(rv) => self.eval_infix_string_concat(op, lv, rv),
                _ => Err(RuntimeError::InfixOpWithInvalidTypes),
            },
            Object::String(lv) => match r {
                Object::String(rv) => self.eval_infix_string(op, lv.clone(), rv.clone()),
                _ => Err(RuntimeError::InfixOpWithInvalidTypes),
            },
            _ => Ok(Object::None),
        }
    }

    fn eval_prefix(&self, op: &PrefixOp, r: &Object) -> Result<Object> {
        match r {
            Object::Bool(b) => self.eval_prefix_bool(op, *b),
            Object::Float(f) => self.eval_prefix_float(op, *f),
            _ => Err(RuntimeError::InfixOpWithInvalidTypes),
        }
    }

    fn eval_prefix_bool(&self, op: &PrefixOp, r: bool) -> Result<Object> {
        Ok(match op {
            PrefixOp::Not => Object::Bool(!r),
            _ => return Err(RuntimeError::InfixOpWithInvalidTypes),
        })
    }

    fn eval_prefix_float(&self, op: &PrefixOp, r: f64) -> Result<Object> {
        Ok(match op {
            PrefixOp::Neg => Object::Float(-r),
            PrefixOp::Pos => Object::Float(r),
            _ => return Err(RuntimeError::InfixOpWithInvalidTypes),
        })
    }

    fn eval_infix_bool(&self, op: &InfixOp, lv: bool, rv: bool) -> Result<Object> {
        Ok(match op {
            InfixOp::Equal => Object::Bool(lv == rv),
            InfixOp::GreaterThan => Object::Bool(lv > rv),
            InfixOp::GreaterThanEqual => Object::Bool(lv >= rv),
            InfixOp::LessThan => Object::Bool(lv < rv),
            InfixOp::LessThanEqual => Object::Bool(lv <= rv),
            InfixOp::NotEqual => Object::Bool(lv != rv),
            _ => return Err(RuntimeError::InfixOpWithInvalidTypes),
        })
    }

    fn eval_infix_float(&self, op: &InfixOp, lv: f64, rv: f64) -> Result<Object> {
        Ok(match op {
            InfixOp::Sum => Object::Float(lv + rv),
            InfixOp::Sub => Object::Float(lv - rv),
            InfixOp::Div => Object::Float(lv / rv),
            InfixOp::Mul => Object::Float(lv * rv),
            InfixOp::Mod => Object::Float(lv % rv),
            InfixOp::Pow => Object::Float(lv.powf(rv)),
            InfixOp::Equal => Object::Bool(lv == rv),
            InfixOp::NotEqual => Object::Bool(lv != rv),
            InfixOp::GreaterThan => Object::Bool(lv > rv),
            InfixOp::GreaterThanEqual => Object::Bool(lv >= rv),
            InfixOp::LessThan => Object::Bool(lv < rv),
            InfixOp::LessThanEqual => Object::Bool(lv <= rv),
            _ => return Err(RuntimeError::InfixOpWithInvalidTypes),
        })
    }

    fn eval_infix_string(&self, op: &InfixOp, lv: String, rv: String) -> Result<Object> {
        todo!()
    }

    pub fn main_proc(&self) -> Option<&[Statement]> {
        for stmt in &self.program {
            match stmt {
                Statement::Procedure { ident, body, .. } => {
                    if ident == "main" {
                        return Some(body.as_ref());
                    }
                }
                _ => {}
            };
        }
        return None;
    }
}
