use std::collections::HashMap;

use super::object::Object;

pub fn new_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert(String::from("len"), Object::Builtin(1, len));
    builtins.insert(String::from("first"), Object::Builtin(1, first));
    builtins.insert(String::from("last"), Object::Builtin(1, last));
    builtins.insert(String::from("rest"), Object::Builtin(1, rest));
    builtins.insert(String::from("push"), Object::Builtin(2, push));
    builtins
}

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments, expected 1, got {}",
            args.len()
        ));
    }
    match &args[0] {
        Object::String(s) => Object::Int(s.len() as i64),
        Object::Array(arr) => Object::Int(arr.len() as i64),
        obj => Object::Error(format!("argument to `len` must be array, got {}", obj)),
    }
}

fn first(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(o) => {
            if let Some(ao) = o.first() {
                ao.clone()
            } else {
                Object::Null
            }
        }
        o => Object::Error(format!("argument to `first` must be array, got {}", o)),
    }
}

fn last(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(o) => {
            if let Some(ao) = o.last() {
                ao.clone()
            } else {
                Object::Null
            }
        }
        o => Object::Error(format!("argument to `last` must be array, got {}", o)),
    }
}

fn rest(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(o) => {
            if o.len() > 0 {
                Object::Array(o[1..].to_vec())
            } else {
                Object::Null
            }
        }
        o => Object::Error(format!("argument to `rest` must be array, got {}", o)),
    }
}

fn push(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(o) => {
            let mut result = o.clone();
            result.push(args[1].clone());
            Object::Array(result)
        }
        o => Object::Error(format!("argument to `push` must be array, got {}", o)),
    }
}
