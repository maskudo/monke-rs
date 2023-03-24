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
    Index(Box<Expr>, Box<Expr>),
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
    Array(Vec<Expr>),
    Hash(Vec<(Expr, Expr)>),
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
    INDEX,       // array[index]
}

impl Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Divide => write!(f, "/"),
            Infix::Multiply => write!(f, "*"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::GreaterThan => write!(f, ">"),
            Infix::LessThan => write!(f, "<"),
            Infix::GreaterEqual => write!(f, ">="),
            Infix::LessEqual => write!(f, "<="),
        }
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::Plus => write!(f, "+"),
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
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
                Literal::Array(exprs) => {
                    write!(f, "{}", Literal::Array(exprs.clone()))
                    // write!(f, "[")?;
                    // for expr in exprs.iter() {
                    //     write!(f, "{expr},")?;
                    // }
                    // write!(f, "]")
                }
                Literal::Hash(exprs) => {
                    write!(f, "{}", Literal::Hash(exprs.clone()))
                }
            },
            Expr::Ident(ident) => {
                write!(f, "{ident}")
            }
            Expr::Infix(left, sym, right) => {
                write!(f, "({left} {sym} {right})")
            }
            Expr::Prefix(sym, right) => {
                write!(f, "({sym}{right})")
            }
            _ => {
                write!(f, "{:?}", self)
            }
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{i}"),
            Literal::String(s) => write!(f, "{s}"),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::Array(exprs) => {
                write!(f, "[")?;
                for expr in exprs.iter() {
                    write!(f, "{expr},")?;
                }
                write!(f, "]")
            }
            Literal::Hash(exprs) => {
                // write!(f, "[{exprs}]")
                write!(f, "{{")?;
                for expr in exprs.iter() {
                    write!(f, "{} : {}", expr.0, expr.1)?;
                }
                write!(f, "}}")
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
