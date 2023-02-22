use std::fmt::{Display, Formatter};

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    String(String),
    Bool(bool),
    ReturnValue(Box<Object>),
    Null,
    Error(String),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match *self {
            Object::Int(ref value) => write!(f, "{}", value),
            Object::Bool(ref value) => write!(f, "{}", value),
            Object::String(ref value) => write!(f, "{}", value),
            Object::Error(ref error) => write!(f, "{}", error),
            _ => write!(f, "null"),
        }
    }
}
