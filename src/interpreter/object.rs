use std::fmt;

#[derive(Debug, Clone)]
pub enum Object {
    Float(f64),
    String(String),
    Bool(bool),
    None,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::String(string) => write!(f, "{}", string),
            Object::Float(float) => write!(f, "{}", float),
            Object::Bool(boolean) => write!(f, "{}", boolean),
            Object::None => write!(f, "None"),
        }
    }
}

