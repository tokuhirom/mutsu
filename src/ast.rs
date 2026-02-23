use crate::token_kind::TokenKind;
use crate::value::Value;

#[derive(Debug, Clone)]
pub(crate) struct ParamDef {
    pub(crate) name: String,
    pub(crate) default: Option<Expr>,
    pub(crate) named: bool,
    pub(crate) slurpy: bool,
    #[allow(dead_code)]
    pub(crate) sigilless: bool,
    pub(crate) type_constraint: Option<String>,
    pub(crate) literal_value: Option<Value>,
    #[allow(dead_code)]
    pub(crate) sub_signature: Option<Vec<ParamDef>>,
    #[allow(dead_code)]
    pub(crate) where_constraint: Option<Box<Expr>>,
    #[allow(dead_code)]
    pub(crate) traits: Vec<String>,
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
    HyperWhatever,
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
    HyperMethodCall {
        target: Box<Expr>,
        name: String,
        args: Vec<Expr>,
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
    /// Array constructed with [...] (reports as "Array" type vs "List" for comma lists).
    BracketArray(Vec<Expr>),
    /// Capture literal: \(positional..., named...) — mixed exprs separated at compile time
    CaptureLiteral(Vec<Expr>),
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
    IndirectTypeLookup(Box<Expr>),
    IndirectCodeLookup {
        package: Box<Expr>,
        name: String,
    },
    PseudoStash(String),
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
    Named {
        name: String,
        value: Option<Expr>,
    },
    /// Capture slip: `|c` — flatten a capture variable into the argument list
    Slip(Expr),
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    VarDecl {
        name: String,
        expr: Expr,
        type_constraint: Option<String>,
        is_state: bool,
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
        is_export: bool,
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
    Fail(Expr),
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
        #[allow(dead_code)]
        is_rw: bool,
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
        predicate: Option<Expr>,
    },
    Phaser {
        kind: PhaserKind,
        body: Vec<Stmt>,
    },
    ProtoDecl {
        name: String,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        is_export: bool,
    },
    Let {
        name: String,
        index: Option<Box<Expr>>,
        value: Option<Box<Expr>>,
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

/// Scan a statement list for placeholder variables (names starting with `^`,
/// e.g. `$^a`, `$^b`) and return their sorted, deduplicated names.
///
/// This is used at parse time to detect implicit block parameters so that
/// constructs like `{ $^a + $^b }` automatically introduce parameters.
pub(crate) fn collect_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_ph_stmt(stmt, &mut names);
    }
    names.sort();
    names.dedup();
    names
}

fn collect_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
            collect_ph_expr(e, out);
        }
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => collect_ph_expr(expr, out),
        Stmt::Say(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_ph_expr(cond, out);
            for s in then_branch {
                collect_ph_stmt(s, out);
            }
            for s in else_branch {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::For { iterable, body, .. } => {
            collect_ph_expr(iterable, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Loop { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::React { body } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Whenever { supply, body, .. } => {
            collect_ph_expr(supply, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Block(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Phaser { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Given { topic, body } => {
            collect_ph_expr(topic, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::When { cond, body } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Let { value, index, .. } => {
            if let Some(e) = value {
                collect_ph_expr(e, out);
            }
            if let Some(e) = index {
                collect_ph_expr(e, out);
            }
        }
        Stmt::ProtoDecl { .. } => {}
        Stmt::DoesDecl { .. } => {}
        Stmt::SubsetDecl {
            predicate: Some(predicate),
            ..
        } => {
            collect_ph_expr(predicate, out);
        }
        Stmt::SubsetDecl {
            predicate: None, ..
        } => {}
        _ => {}
    }
}

fn collect_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Var(name) if name.starts_with('^') => {
            if !out.contains(name) {
                out.push(name.clone());
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => collect_ph_expr(expr, out),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::CallOn { target, args } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Index { target, index } => {
            collect_ph_expr(target, out);
            collect_ph_expr(index, out);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_ph_expr(cond, out);
            collect_ph_expr(then_expr, out);
            collect_ph_expr(else_expr, out);
        }
        Expr::AssignExpr { expr, .. } | Expr::Exists(expr) => collect_ph_expr(expr, out),
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Expr::Block(stmts)
        | Expr::AnonSub(stmts)
        | Expr::AnonSubParams { body: stmts, .. }
        | Expr::Gather(stmts) => {
            for s in stmts {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoBlock { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoStmt(stmt) => {
            collect_ph_stmt(stmt, out);
        }
        Expr::Lambda { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::Try { body, catch } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
            if let Some(c) = catch {
                for s in c {
                    collect_ph_stmt(s, out);
                }
            }
        }
        Expr::CodeVar(_) => {}
        Expr::IndirectCodeLookup { package, .. } => collect_ph_expr(package, out),
        Expr::Reduction { expr, .. } => collect_ph_expr(expr, out),
        Expr::HyperOp { left, right, .. } | Expr::MetaOp { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::InfixFunc { left, right, .. } => {
            collect_ph_expr(left, out);
            for e in right {
                collect_ph_expr(e, out);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    collect_ph_expr(e, out);
                }
            }
        }
        _ => {}
    }
}

/// Create an `Expr::AnonSub` or `Expr::AnonSubParams` depending on whether
/// the block body contains placeholder variables (`$^a`, `$^b`, etc.).
pub(crate) fn make_anon_sub(stmts: Vec<Stmt>) -> Expr {
    let placeholders = collect_placeholders(&stmts);
    if placeholders.is_empty() {
        Expr::AnonSub(stmts)
    } else {
        Expr::AnonSubParams {
            params: placeholders,
            body: stmts,
        }
    }
}
