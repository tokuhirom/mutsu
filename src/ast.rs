use crate::token_kind::TokenKind;
use crate::value::Value;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub(crate) struct ParamDef {
    pub(crate) name: String,
    pub(crate) default: Option<Expr>,
    pub(crate) multi_invocant: bool,
    pub(crate) required: bool,
    pub(crate) named: bool,
    pub(crate) slurpy: bool,
    pub(crate) double_slurpy: bool,
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
    pub(crate) optional_marker: bool,
    pub(crate) outer_sub_signature: Option<Vec<ParamDef>>,
    pub(crate) code_signature: Option<(Vec<ParamDef>, Option<String>)>,
    /// True when this parameter is the explicit invocant (e.g. `$self:` in a method signature).
    pub(crate) is_invocant: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionDef {
    pub(crate) package: String,
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) body: Vec<Stmt>,
    pub(crate) is_test_assertion: bool,
    /// When true, this sub has an explicit empty signature `()` and should reject any arguments.
    pub(crate) empty_sig: bool,
}

pub(crate) fn function_body_fingerprint(
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> u64 {
    let mut hasher = DefaultHasher::new();
    format!("{:?}", params).hash(&mut hasher);
    format!("{:?}", param_defs).hash(&mut hasher);
    format!("{:?}", body).hash(&mut hasher);
    hasher.finish()
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
        samemark: bool,
    },
    NonDestructiveSubst {
        pattern: String,
        replacement: String,
        samemark: bool,
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
        /// True when the method name was quoted (e.g. `."DEFINITE"()`),
        /// which bypasses pseudo-method macros like .DEFINITE, .WHAT, etc.
        quoted: bool,
    },
    DynamicMethodCall {
        target: Box<Expr>,
        name_expr: Box<Expr>,
        args: Vec<Expr>,
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
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
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
    /// Invocant colon: `foo($obj:)` — call sub `foo` as a method on `$obj`
    Invocant(Expr),
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    VarDecl {
        name: String,
        expr: Expr,
        type_constraint: Option<String>,
        is_state: bool,
        is_our: bool,
    },
    Assign {
        name: String,
        expr: Expr,
        op: AssignOp,
    },
    SubDecl {
        name: String,
        name_expr: Option<Expr>,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        return_type: Option<String>,
        signature_alternates: Vec<(Vec<String>, Vec<ParamDef>)>,
        body: Vec<Stmt>,
        multi: bool,
        is_export: bool,
        is_test_assertion: bool,
        supersede: bool,
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
        param_def: Box<Option<ParamDef>>,
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
    /// `no Module ...;` — disable pragma/module effects for current lexical scope.
    No {
        module: String,
    },
    /// `need Module;` — load module without importing exports
    Need {
        module: String,
    },
    Subtest {
        name: Expr,
        body: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
    /// Non-lexical statement sequence used by parser desugarings.
    SyntheticBlock(Vec<Stmt>),
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
        name_expr: Option<Expr>,
        parents: Vec<String>,
        is_hidden: bool,
        hidden_parents: Vec<String>,
        body: Vec<Stmt>,
    },
    HasDecl {
        name: String,
        is_public: bool,
        default: Option<Expr>,
        handles: Vec<String>,
        #[allow(dead_code)]
        is_rw: bool,
    },
    MethodDecl {
        name: String,
        name_expr: Option<Expr>,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        multi: bool,
        is_rw: bool,
        is_private: bool,
        is_our: bool,
        return_type: Option<String>,
    },
    RoleDecl {
        name: String,
        type_params: Vec<String>,
        body: Vec<Stmt>,
    },
    DoesDecl {
        name: String,
    },
    TrustsDecl {
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
    TempMethodAssign {
        var_name: String,
        method_name: String,
        method_args: Vec<Expr>,
        value: Expr,
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
    // Sort by the name component (strip & and ^ prefixes) so that
    // $^a, $^b, &^c sort as a, b, c regardless of sigil.
    names.sort_by(|a, b| {
        let a_name = a.trim_start_matches('&').trim_start_matches('^');
        let b_name = b.trim_start_matches('&').trim_start_matches('^');
        a_name.cmp(b_name)
    });
    names.dedup();
    names
}

fn collect_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
            collect_ph_expr(e, out);
        }
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => collect_ph_expr(expr, out),
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) => collect_ph_expr(e, out),
                    CallArg::Named { value: Some(e), .. } => collect_ph_expr(e, out),
                    CallArg::Named { value: None, .. } => {}
                    CallArg::Slip(e) => collect_ph_expr(e, out),
                }
            }
        }
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
        | Stmt::SyntheticBlock(body)
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
        Stmt::TempMethodAssign {
            method_args, value, ..
        } => {
            for e in method_args {
                collect_ph_expr(e, out);
            }
            collect_ph_expr(value, out);
        }
        Stmt::ProtoDecl { .. } => {}
        Stmt::DoesDecl { .. } => {}
        Stmt::TrustsDecl { .. } => {}
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
        Expr::Var(name) if name.starts_with('^') || name.starts_with(':') => {
            if !out.contains(name) {
                out.push(name.clone());
            }
        }
        Expr::CodeVar(name) if name.starts_with('^') => {
            let prefixed = format!("&{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => collect_ph_expr(expr, out),
        Expr::MethodCall { target, args, .. }
        | Expr::DynamicMethodCall { target, args, .. }
        | Expr::HyperMethodCall { target, args, .. } => {
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
        Expr::CodeVar(name) if name.starts_with('^') => {
            let prefixed = format!("&{}", name);
            if !out.contains(&prefixed) {
                out.push(prefixed);
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

/// Check if a bare variable reference ($name or $name = ...) appears before
/// the corresponding placeholder variable ($^name) in statement order.
/// Used to detect X::Undeclared and X::Placeholder::NonPlaceholder.
pub(crate) fn bare_precedes_placeholder(stmts: &[Stmt], bare_name: &str) -> bool {
    let ph_name = format!("^{}", bare_name);
    let mut ph_seen = false;
    for stmt in stmts {
        if stmt_contains_var_named(stmt, &ph_name) {
            ph_seen = true;
        }
        if !ph_seen && stmt_references_bare(stmt, bare_name) {
            return true;
        }
    }
    false
}

/// Check if a statement contains a variable reference Var(name).
fn stmt_contains_var_named(stmt: &Stmt, var_name: &str) -> bool {
    let mut found = false;
    check_bare_var_stmt(stmt, var_name, &mut found);
    found
}

/// Check if a statement references a bare variable (Var(name) or Assign{name}).
fn stmt_references_bare(stmt: &Stmt, bare_name: &str) -> bool {
    // Check assignment target name
    if let Stmt::Assign { name, .. } = stmt
        && name == bare_name
    {
        return true;
    }
    let mut found = false;
    check_bare_var_stmt(stmt, bare_name, &mut found);
    found
}

pub(crate) fn has_var_decl(stmts: &[Stmt], name: &str) -> bool {
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl {
                name: decl_name, ..
            } if decl_name == name => return true,
            _ => {}
        }
    }
    false
}

fn check_bare_var_stmt(stmt: &Stmt, bare_name: &str, found: &mut bool) {
    match stmt {
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
            check_bare_var_expr(e, bare_name, found);
        }
        Stmt::Assign { expr, .. } => check_bare_var_expr(expr, bare_name, found),
        Stmt::Say(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                check_bare_var_expr(e, bare_name, found);
            }
        }
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) => {
                        check_bare_var_expr(e, bare_name, found)
                    }
                    CallArg::Named { value: Some(e), .. } => {
                        check_bare_var_expr(e, bare_name, found)
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
}

fn check_bare_var_expr(expr: &Expr, bare_name: &str, found: &mut bool) {
    match expr {
        Expr::Var(name) if name == bare_name => *found = true,
        Expr::Binary { left, right, .. } => {
            check_bare_var_expr(left, bare_name, found);
            check_bare_var_expr(right, bare_name, found);
        }
        Expr::Unary { expr, .. } => check_bare_var_expr(expr, bare_name, found),
        Expr::MethodCall { target, args, .. } | Expr::DynamicMethodCall { target, args, .. } => {
            check_bare_var_expr(target, bare_name, found);
            for a in args {
                check_bare_var_expr(a, bare_name, found);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                check_bare_var_expr(a, bare_name, found);
            }
        }
        Expr::StringInterpolation(parts) => {
            for p in parts {
                check_bare_var_expr(p, bare_name, found);
            }
        }
        Expr::AssignExpr { expr, .. } => check_bare_var_expr(expr, bare_name, found),
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
            params: placeholders.clone(),
            param_defs: placeholders
                .iter()
                .map(|name| ParamDef {
                    name: name.clone(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    double_slurpy: false,
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                })
                .collect(),
            return_type: None,
            body: stmts,
        }
    }
}
