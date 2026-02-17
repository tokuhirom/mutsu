use crate::token_kind::TokenKind;
use crate::value::Value;

#[derive(Debug, Clone)]
pub(crate) struct ParamDef {
    pub(crate) name: String,
    pub(crate) default: Option<Expr>,
    pub(crate) named: bool,
    pub(crate) slurpy: bool,
    pub(crate) type_constraint: Option<String>,
    pub(crate) literal_value: Option<Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionDef {
    pub(crate) package: String,
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum PhaserKind {
    Begin,
    Check,
    Init,
    End,
    Enter,
    Leave,
    Keep,
    Undo,
    First,
    Next,
    Last,
    Pre,
    Post,
    Quit,
    Close,
}

#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names, dead_code)]
pub(crate) enum Expr {
    Literal(Value),
    Whatever,
    BareWord(String),
    StringInterpolation(Vec<Expr>),
    Var(String),
    CaptureVar(String),
    ArrayVar(String),
    HashVar(String),
    CodeVar(String),
    EnvIndex(String),
    Subst {
        pattern: String,
        replacement: String,
    },
    NonDestructiveSubst {
        pattern: String,
        replacement: String,
    },
    Transliterate {
        from: String,
        to: String,
    },
    MethodCall {
        target: Box<Expr>,
        name: String,
        args: Vec<Expr>,
        modifier: Option<char>,
    },
    Exists(Box<Expr>),
    RoutineMagic,
    BlockMagic,
    Block(Vec<Stmt>),
    AnonSub(Vec<Stmt>),
    AnonSubParams {
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    CallOn {
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    Lambda {
        param: String,
        body: Vec<Stmt>,
    },
    ArrayLiteral(Vec<Expr>),
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    IndexAssign {
        target: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    },
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    AssignExpr {
        name: String,
        expr: Box<Expr>,
    },
    Unary {
        op: TokenKind,
        expr: Box<Expr>,
    },
    PostfixOp {
        op: TokenKind,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: TokenKind,
        right: Box<Expr>,
    },
    Hash(Vec<(String, Option<Expr>)>),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Try {
        body: Vec<Stmt>,
        catch: Option<Vec<Stmt>>,
    },
    Gather(Vec<Stmt>),
    Reduction {
        op: String,
        expr: Box<Expr>,
    },
    InfixFunc {
        name: String,
        left: Box<Expr>,
        right: Vec<Expr>,
        modifier: Option<String>,
    },
    HyperOp {
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
        dwim_left: bool,
        dwim_right: bool,
    },
    MetaOp {
        meta: String, // "R", "X", "Z"
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    DoBlock {
        body: Vec<Stmt>,
        label: Option<String>,
    },
    DoStmt(Box<Stmt>),
    ControlFlow {
        kind: ControlFlowKind,
        label: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum ControlFlowKind {
    Last,
    Next,
    Redo,
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
    VarDecl {
        name: String,
        expr: Expr,
        type_constraint: Option<String>,
    },
    Assign {
        name: String,
        expr: Expr,
        op: AssignOp,
    },
    SubDecl {
        name: String,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
    },
    TokenDecl {
        name: String,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
    },
    RuleDecl {
        name: String,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
    },
    #[allow(dead_code)]
    ProtoToken {
        name: String,
    },
    Package {
        name: String,
        body: Vec<Stmt>,
    },
    Return(Expr),
    For {
        iterable: Expr,
        param: Option<String>,
        params: Vec<String>,
        body: Vec<Stmt>,
        label: Option<String>,
    },
    Say(Vec<Expr>),
    Print(Vec<Expr>),
    Note(Vec<Expr>),
    Call {
        name: String,
        args: Vec<CallArg>,
    },
    Use {
        module: String,
        arg: Option<Expr>,
    },
    Subtest {
        name: Expr,
        body: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
    If {
        cond: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Vec<Stmt>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
        label: Option<String>,
    },
    Loop {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Vec<Stmt>,
        repeat: bool,
        label: Option<String>,
    },
    React {
        body: Vec<Stmt>,
    },
    Whenever {
        supply: Expr,
        param: Option<String>,
        body: Vec<Stmt>,
    },
    Last(Option<String>),
    Next(Option<String>),
    Redo(Option<String>),
    Proceed,
    Succeed,
    Given {
        topic: Expr,
        body: Vec<Stmt>,
    },
    When {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Default(Vec<Stmt>),
    Die(Expr),
    Catch(Vec<Stmt>),
    Control(Vec<Stmt>),
    Take(Expr),
    EnumDecl {
        name: String,
        variants: Vec<(String, Option<Expr>)>,
    },
    ClassDecl {
        name: String,
        parents: Vec<String>,
        body: Vec<Stmt>,
    },
    HasDecl {
        name: String,
        is_public: bool,
        default: Option<Expr>,
    },
    MethodDecl {
        name: String,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
    },
    RoleDecl {
        name: String,
        body: Vec<Stmt>,
    },
    DoesDecl {
        name: String,
    },
    SubsetDecl {
        name: String,
        base: String,
        predicate: Expr,
    },
    Phaser {
        kind: PhaserKind,
        body: Vec<Stmt>,
    },
    ProtoDecl {
        name: String,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum AssignOp {
    Assign,
    Bind,
    #[allow(dead_code)]
    MatchAssign,
}
