use std::collections::HashMap;

use super::object::Object;

pub fn new_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert(String::from("len"), Object::Builtin(1, len));
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
        obj => Object::Error(format!("argument to `len` not supported, got {}", obj)),
    }
}
