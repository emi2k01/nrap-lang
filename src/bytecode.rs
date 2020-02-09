pub enum Instruction {
    LoadConst {
        value: Constant,
    },
    Pop,
}

pub enum Constant {
    Number(f64),
    String(String),
    Bool(bool),
}
