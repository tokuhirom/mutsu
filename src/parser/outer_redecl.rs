//! Post-parse check for redeclaring a lexical that is already bound to an outer
//! symbol *after it has been referenced* in the current scope.
//!
//! Rakudo raises a compile-time `X::Redeclaration::Outer`
//! ("Lexical symbol '$x' is already bound to an outer symbol...") when, within a
//! single lexical scope, a name is first *referenced* — resolving to a binding in
//! an enclosing scope — and then *redeclared* with `my`/`state`. For example:
//!
//! ```raku
//! sub s($i is copy) {
//!     for 1..3 {
//!         @array.push($i);   # references the outer (parameter) $i
//!         my $i = 1;         # ERROR: $i is already bound to an outer symbol
//!     }
//! }
//! ```
//!
//! The error fires only when *all three* hold in the same scope, in order:
//!   1. the name is declared in an *enclosing* scope (a `my`/`state`/param), and
//!   2. it is referenced in this scope *before* being redeclared here, and
//!   3. it is then redeclared with `my`/`state` in this scope.
//!
//! A same-scope redeclaration (no enclosing binding) is only a warning, and a
//! redeclaration *before* any reference is fine — so this walker tracks, per
//! lexical scope, the set of names referenced-as-outer so far, and checks each
//! `my`/`state` declaration against it.
//!
//! The walker is deliberately conservative: any construct it does not descend
//! into simply drops references (a false *negative*). A false *positive* is only
//! possible if an in-scope declaration is missed, so every declaration form
//! (`my`/`state`, params, `for`/pointy loop variables, and inline `do my $x`) is
//! registered before the following statements are examined.

use crate::ast::{Expr, ParamDef, Stmt};
use std::collections::HashSet;

/// A single lexical scope: names declared here so far, and names referenced here
/// that resolved to an enclosing scope (before any local redeclaration).
struct Scope {
    declared: HashSet<String>,
    ref_outer: HashSet<String>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            declared: HashSet::new(),
            ref_outer: HashSet::new(),
        }
    }
}

struct Ctx {
    scopes: Vec<Scope>,
    line: i64,
    /// The first offending `(sigil+name, line)` found, if any.
    found: Option<(String, i64)>,
}

impl Ctx {
    /// Register a reference to `key` in the current (innermost) scope. If the
    /// name is not declared here but *is* declared in some enclosing scope, it is
    /// an outer reference and recorded as such.
    fn reference(&mut self, key: String) {
        let depth = self.scopes.len();
        if depth == 0 {
            return;
        }
        if self.scopes[depth - 1].declared.contains(&key) {
            return; // resolves locally
        }
        let outer = self.scopes[..depth - 1]
            .iter()
            .any(|s| s.declared.contains(&key));
        if outer {
            self.scopes[depth - 1].ref_outer.insert(key);
        }
    }

    /// Register a `my`/`state` declaration of `key` in the current scope. If it
    /// was already referenced-as-outer here, that is the error.
    fn declare(&mut self, key: String) {
        let depth = self.scopes.len();
        if depth == 0 {
            return;
        }
        if self.found.is_none() && self.scopes[depth - 1].ref_outer.contains(&key) {
            self.found = Some((key.clone(), self.line));
        }
        self.scopes[depth - 1].declared.insert(key);
    }

    /// Register a declared name (param / loop var) without the outer-reference
    /// check — used when seeding a fresh scope with its parameters.
    fn seed(&mut self, key: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.declared.insert(key);
        }
    }
}

/// Normalize a declaration/param name (`"i"`, `"@b"`, `"%c"`) to a sigil-keyed
/// form (`"$i"`, `"@b"`, `"%c"`), or `None` if it is a special/synthetic name we
/// do not track.
fn decl_key(name: &str) -> Option<String> {
    let (sigil, base) = match name.chars().next()? {
        '@' => ('@', &name[1..]),
        '%' => ('%', &name[1..]),
        '$' => ('$', &name[1..]),
        '&' => return None, // subs are hoisted; not subject to this rule
        _ => ('$', name),
    };
    normalized(sigil, base)
}

/// Build the tracked key for a reference of the given sigil and base name.
fn ref_key(sigil: char, base: &str) -> Option<String> {
    normalized(sigil, base)
}

fn normalized(sigil: char, base: &str) -> Option<String> {
    let first = base.chars().next()?;
    // Reject twigils, special variables, package-qualified names and synthetic
    // compiler-generated temporaries.
    if !(first.is_ascii_alphabetic() || first == '_') {
        return None;
    }
    if base == "_" || base.starts_with("__") || base.contains(':') {
        return None;
    }
    Some(format!("{}{}", sigil, base))
}

/// Returns `(sigil+name, line)` for the first `my`/`state` redeclaration of a
/// referenced outer symbol, or `None` if there is no such offense.
pub(crate) fn find_outer_redeclaration(stmts: &[Stmt]) -> Option<(String, i64)> {
    let mut ctx = Ctx {
        scopes: vec![Scope::new()],
        line: 0,
        found: None,
    };
    walk_stmts(stmts, &mut ctx);
    ctx.found
}

fn seed_params(params: &[String], param_defs: &[ParamDef], ctx: &mut Ctx) {
    for p in params {
        if let Some(k) = decl_key(p) {
            ctx.seed(k);
        }
    }
    for d in param_defs {
        if let Some(k) = decl_key(&d.name) {
            ctx.seed(k);
        }
    }
}

fn walk_scoped_body(body: &[Stmt], ctx: &mut Ctx) {
    ctx.scopes.push(Scope::new());
    walk_stmts(body, ctx);
    ctx.scopes.pop();
}

fn walk_scoped_body_with_params(
    body: &[Stmt],
    params: &[String],
    param_defs: &[ParamDef],
    ctx: &mut Ctx,
) {
    ctx.scopes.push(Scope::new());
    seed_params(params, param_defs, ctx);
    walk_stmts(body, ctx);
    ctx.scopes.pop();
}

fn walk_stmts(stmts: &[Stmt], ctx: &mut Ctx) {
    for s in stmts {
        walk_stmt(s, ctx);
    }
}

fn walk_stmt(stmt: &Stmt, ctx: &mut Ctx) {
    match stmt {
        Stmt::SetLine(n) => ctx.line = *n,

        Stmt::VarDecl {
            name, expr, is_our, ..
        } => {
            if let Some(key) = decl_key(name) {
                if *is_our {
                    // `our` is package-scoped and not subject to the outer-binding
                    // rule; still record it so later references resolve locally.
                    ctx.seed(key);
                } else {
                    ctx.declare(key);
                }
            }
            // The declared name is in scope for its own initializer, so walk the
            // RHS *after* declaring (a self-reference is then local, not outer).
            walk_expr(expr, ctx);
        }

        Stmt::Assign { name, expr, .. } => {
            if let Some(key) = decl_key(name) {
                ctx.reference(key);
            }
            walk_expr(expr, ctx);
        }

        Stmt::Return(e) | Stmt::Take(e, _) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Goto(e) => {
            walk_expr(e, ctx)
        }
        Stmt::Expr(e) => walk_expr(e, ctx),

        // I/O statements carry their operands as a plain expression list.
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                walk_expr(e, ctx);
            }
        }
        Stmt::Call { args, .. } => {
            for a in args {
                walk_call_arg(a, ctx);
            }
        }
        Stmt::Phaser { body, .. } => walk_scoped_body(body, ctx),

        // Routine boundaries: a fresh scope seeded with the parameters. Closures
        // can still see outer lexicals, so this is a normal nested scope.
        Stmt::SubDecl {
            body,
            params,
            param_defs,
            ..
        } => walk_scoped_body_with_params(body, params, param_defs, ctx),
        Stmt::MethodDecl {
            body,
            params,
            param_defs,
            ..
        } => walk_scoped_body_with_params(body, params, param_defs, ctx),
        Stmt::TokenDecl { body, .. } | Stmt::RuleDecl { body, .. } => walk_scoped_body(body, ctx),

        Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } | Stmt::Package { body, .. } => {
            walk_scoped_body(body, ctx)
        }

        // A `SyntheticBlock` is a compiler-internal grouping (e.g. the lowering of
        // `my ($a, $b) = ...` destructuring). Its declarations belong to the
        // enclosing lexical scope, so it does not open a new scope.
        Stmt::SyntheticBlock(body) => walk_stmts(body, ctx),

        // Inline blocks open a new lexical scope but preserve outer visibility.
        Stmt::Block(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::React { body }
        | Stmt::Subtest { body, .. } => walk_scoped_body(body, ctx),

        Stmt::Given { body, topic, .. } => {
            walk_expr(topic, ctx);
            walk_scoped_body(body, ctx);
        }
        Stmt::When { cond, body } => {
            walk_expr(cond, ctx);
            walk_scoped_body(body, ctx);
        }
        Stmt::Whenever { supply, body, .. } => {
            walk_expr(supply, ctx);
            walk_scoped_body(body, ctx);
        }
        Stmt::While { cond, body, .. } => {
            walk_expr(cond, ctx);
            walk_scoped_body(body, ctx);
        }
        Stmt::For {
            iterable,
            params,
            params_def,
            param,
            param_def,
            body,
            ..
        } => {
            walk_expr(iterable, ctx);
            ctx.scopes.push(Scope::new());
            if let Some(p) = param
                && let Some(k) = decl_key(p)
            {
                ctx.seed(k);
            }
            if let Some(d) = param_def.as_ref().as_ref()
                && let Some(k) = decl_key(&d.name)
            {
                ctx.seed(k);
            }
            seed_params(params, params_def, ctx);
            walk_stmts(body, ctx);
            ctx.scopes.pop();
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            ..
        } => {
            // A C-style loop's `init` declarations share the loop's scope.
            ctx.scopes.push(Scope::new());
            if let Some(init) = init {
                walk_stmt(init, ctx);
            }
            if let Some(c) = cond {
                walk_expr(c, ctx);
            }
            if let Some(s) = step {
                walk_expr(s, ctx);
            }
            walk_stmts(body, ctx);
            ctx.scopes.pop();
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            walk_expr(cond, ctx);
            walk_scoped_body(then_branch, ctx);
            // `elsif` chains are represented as a nested `If` inside `else_branch`.
            walk_scoped_body(else_branch, ctx);
        }
        Stmt::Label { stmt, .. } => walk_stmt(stmt, ctx),

        _ => {}
    }
}

fn walk_call_arg(arg: &crate::ast::CallArg, ctx: &mut Ctx) {
    use crate::ast::CallArg;
    match arg {
        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => walk_expr(e, ctx),
        CallArg::Named { value, .. } => {
            if let Some(v) = value {
                walk_expr(v, ctx);
            }
        }
    }
}

fn walk_expr(expr: &Expr, ctx: &mut Ctx) {
    match expr {
        Expr::Var(n) => {
            if let Some(k) = ref_key('$', n) {
                ctx.reference(k);
            }
        }
        Expr::ArrayVar(n) => {
            if let Some(k) = ref_key('@', n) {
                ctx.reference(k);
            }
        }
        Expr::HashVar(n) => {
            if let Some(k) = ref_key('%', n) {
                ctx.reference(k);
            }
        }

        // `do STMT` (statement form) shares the enclosing scope, so an inline
        // `do my $x = 5` declares in the current scope. `do { ... }` is a
        // separate `DoBlock` variant handled as a nested scope below.
        Expr::DoStmt(s) => walk_stmt(s, ctx),

        Expr::AssignExpr { name, expr, .. } => {
            if let Some(k) = decl_key(name) {
                ctx.reference(k);
            }
            walk_expr(expr, ctx);
        }

        // Body-bearing expressions open a new nested lexical scope.
        Expr::Block(body)
        | Expr::Gather(body)
        | Expr::DoBlock { body, .. }
        | Expr::Once { body }
        | Expr::PhaserExpr { body, .. } => walk_scoped_body(body, ctx),
        Expr::AnonSub { body, .. } => walk_scoped_body(body, ctx),
        Expr::AnonSubParams {
            body,
            params,
            param_defs,
            ..
        } => walk_scoped_body_with_params(body, params, param_defs, ctx),
        Expr::Lambda { param, body, .. } => {
            walk_scoped_body_with_params(body, std::slice::from_ref(param), &[], ctx)
        }
        Expr::Try { body, catch } => {
            walk_scoped_body(body, ctx);
            if let Some(c) = catch {
                walk_scoped_body(c, ctx);
            }
        }

        // Same-scope compound expressions: recurse into children.
        Expr::MethodCall { target, args, .. }
        | Expr::HyperMethodCall { target, args, .. }
        | Expr::CallOn { target, args } => {
            walk_expr(target, ctx);
            for a in args {
                walk_expr(a, ctx);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            walk_expr(target, ctx);
            walk_expr(name_expr, ctx);
            for a in args {
                walk_expr(a, ctx);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                walk_expr(a, ctx);
            }
        }
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            walk_expr(left, ctx);
            walk_expr(right, ctx);
        }
        Expr::InfixFunc { left, right, .. } => {
            walk_expr(left, ctx);
            for r in right {
                walk_expr(r, ctx);
            }
        }
        Expr::Feed { source, sink, .. } => {
            walk_expr(source, ctx);
            walk_expr(sink, ctx);
        }
        Expr::Unary { expr, .. }
        | Expr::PostfixOp { expr, .. }
        | Expr::Eager(expr)
        | Expr::Itemize(expr)
        | Expr::DeitemizeForBind(expr)
        | Expr::Reduction { expr, .. }
        | Expr::ZenSlice(expr)
        | Expr::PositionalPair(expr)
        | Expr::IndirectTypeLookup(expr)
        | Expr::SymbolicDeref { expr, .. } => walk_expr(expr, ctx),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            walk_expr(cond, ctx);
            walk_expr(then_expr, ctx);
            walk_expr(else_expr, ctx);
        }
        Expr::Index { target, index, .. } => {
            walk_expr(target, ctx);
            walk_expr(index, ctx);
        }
        Expr::MultiDimIndex { target, dimensions } => {
            walk_expr(target, ctx);
            for d in dimensions {
                walk_expr(d, ctx);
            }
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
        } => {
            walk_expr(target, ctx);
            for d in dimensions {
                walk_expr(d, ctx);
            }
            walk_expr(value, ctx);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            walk_expr(target, ctx);
            walk_expr(index, ctx);
            walk_expr(value, ctx);
        }
        Expr::Exists { target, arg, .. } => {
            walk_expr(target, ctx);
            if let Some(a) = arg {
                walk_expr(a, ctx);
            }
        }
        Expr::SymbolicDerefAssign { expr, value, .. }
        | Expr::IndirectTypeLookupAssign { expr, value } => {
            walk_expr(expr, ctx);
            walk_expr(value, ctx);
        }
        Expr::HyperSlice { target, .. } => walk_expr(target, ctx),
        Expr::HyperIndex { target, keys } => {
            walk_expr(target, ctx);
            walk_expr(keys, ctx);
        }
        Expr::ArrayLiteral(items) | Expr::BracketArray(items, _) | Expr::CaptureLiteral(items) => {
            for it in items {
                walk_expr(it, ctx);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(v) = v {
                    walk_expr(v, ctx);
                }
            }
        }
        Expr::IndirectCodeLookup { package, .. } => walk_expr(package, ctx),

        _ => {}
    }
}
