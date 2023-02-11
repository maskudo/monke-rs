use core::fmt;
use std::fmt::Display;

#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    Let(Ident, Expr),
    Return(Expr),
    ExprStmt(Expr),
}

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Box<Expr>, Infix, Box<Expr>),
    If {
        condition: Box<Expr>,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    },
    Function {
        parameters: Vec<Ident>,
        body: BlockStmt,
    },
    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterEqual,
    GreaterThan,
    LessEqual,
    LessThan,
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

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    LOWEST,
    EQUALS,      //==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     //*
    PREFIX,      // -x or !x
    CALL,        //myFunc(x)
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
            _ => {
                write!(f, "{:?}", self)
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
            Stmt::ExprStmt(expr) => {
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
