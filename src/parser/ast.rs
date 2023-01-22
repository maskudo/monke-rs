#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    Blank,
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
