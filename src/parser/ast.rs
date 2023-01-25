use core::fmt;
use std::fmt::Display;

#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
}
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Expr {
    IntLiteral(i64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn token_literal(&mut self) -> String {
        todo!()
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::IntLiteral(i) => {
                write!(f, "{i}")
            }
        }
    }
}
impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let(Ident(ident), expr) => {
                write!(f, "let {ident} = {expr};")
            }
            Stmt::Return(expr) => {
                write!(f, "return = {expr};")
            }
            Stmt::Expr(expr) => {
                write!(f, "{expr}")
            }
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{stmt}\n");
        }
        write!(f, "")
    }
}
