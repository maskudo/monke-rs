use super::env::Env;
use crate::parser::ast::{BlockStmt, Ident};
use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};
pub type BuiltInFunc = fn(Vec<Object>) -> Object;
pub type NoOfParams = u8;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    String(String),
    Bool(bool),
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<Ident>,
        body: BlockStmt,
        env: Rc<RefCell<Env>>,
    },
    Builtin(NoOfParams, BuiltInFunc),
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
