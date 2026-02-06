use crate::lexer::TokenKind;
use crate::value::Value;

#[derive(Debug, Clone)]
pub(crate) struct FunctionDef {
    pub(crate) package: String,
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Literal(Value),
    StringInterpolation(Vec<Expr>),
    Var(String),
    ArrayVar(String),
    HashVar(String),
    EnvIndex(String),
    MethodCall { target: Box<Expr>, name: String, args: Vec<Expr> },
    Exists(Box<Expr>),
    RoutineMagic,
    BlockMagic,
    Block(Vec<Stmt>),
    AnonSub(Vec<Stmt>),
    CallOn { target: Box<Expr>, args: Vec<Expr> },
    Lambda { param: String, body: Vec<Stmt> },
    ArrayLiteral(Vec<Expr>),
    Index { target: Box<Expr>, index: Box<Expr> },
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    AssignExpr { name: String, expr: Box<Expr> },
    Unary { op: TokenKind, expr: Box<Expr> },
    PostfixOp { op: TokenKind, expr: Box<Expr> },
    Binary { left: Box<Expr>, op: TokenKind, right: Box<Expr> },
    Hash(Vec<(String, Option<Expr>)>),
    Call { name: String, args: Vec<Expr> },
    Try { body: Vec<Stmt>, catch: Option<Vec<Stmt>> },
    InfixFunc {
        name: String,
        left: Box<Expr>,
        right: Vec<Expr>,
        modifier: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum CallArg {
    Positional(Expr),
    Named { name: String, value: Option<Expr> },
}

#[derive(Debug, Clone)]
pub(crate) enum ExpectedMatcher {
    Exact(Value),
    Lambda { param: String, body: Vec<Stmt> },
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    VarDecl { name: String, expr: Expr },
    Assign { name: String, expr: Expr, op: AssignOp },
    SubDecl { name: String, params: Vec<String>, body: Vec<Stmt> },
    Package { name: String, body: Vec<Stmt> },
    Return(Expr),
    For { iterable: Expr, param: Option<String>, body: Vec<Stmt> },
    Say(Expr),
    Print(Expr),
    Call { name: String, args: Vec<CallArg> },
    Use { module: String, arg: Option<Expr> },
    Subtest { name: Expr, body: Vec<Stmt>, is_sub: bool },
    Block(Vec<Stmt>),
    If { cond: Expr, then_branch: Vec<Stmt>, else_branch: Vec<Stmt> },
    While { cond: Expr, body: Vec<Stmt> },
    Loop {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Vec<Stmt>,
        repeat: bool,
    },
    Last,
    Next,
    Given { topic: Expr, body: Vec<Stmt> },
    When { cond: Expr, body: Vec<Stmt> },
    Default(Vec<Stmt>),
    Die(Expr),
    Catch(Vec<Stmt>),
    Expr(Expr),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum AssignOp {
    Assign,
    Bind,
    MatchAssign,
}
