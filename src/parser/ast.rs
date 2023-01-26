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
#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
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
            Expr::Literal(literal) => match literal {
                Literal::Int(i) => {
                    write!(f, "{i}")
                }
                Literal::String(string) => {
                    write!(f, "{string}")
                }
                Literal::Bool(bool) => {
                    write!(f, "{bool}")
                }
            },
            Expr::Ident(ident) => {
                write!(f, "{ident}")
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
            write!(f, "{stmt}\n")?
        }
        Ok(())
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
