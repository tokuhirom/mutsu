//! Compile-time detection of calls to undeclared routines in the mainline.
//!
//! Rakudo resolves routine names at CHECK time (end of compilation), so a call
//! to a routine that is declared nowhere in the compilation unit aborts before
//! anything runs: `say 42; nosuchsub()` prints nothing and exits with
//! `===SORRY!=== ... Undeclared routine:\n    nosuchsub used at line 1`.
//!
//! The walker is deliberately conservative in the safe direction: declarations
//! are collected *scope-blind* from the whole unit (a sub declared inside any
//! nested block/class/sub suppresses the error even where raku's lexical
//! scoping would not), and the check bails out entirely when the unit imports
//! names it cannot see through (`use`/`need`/`import`/`require`). A missed
//! construct can only produce a false *negative* (the call is then caught at
//! runtime as before), never a false positive, as long as every construct the
//! call-walker descends into also has its declarations collected — both are
//! gathered in the same traversal to keep them symmetric.

use crate::ast::{CallArg, Expr, ParamDef, Stmt};
use crate::value::{RuntimeError, RuntimeErrorCode};
use std::collections::HashSet;

use super::Interpreter;

/// Native (lowercase) type names that may appear in call position as
/// coercions/constructors (`int8(...)`) without being routine declarations.
const NATIVE_TYPE_NAMES: &[&str] = &[
    "int",
    "int8",
    "int16",
    "int32",
    "int64",
    "uint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "num",
    "num32",
    "num64",
    "str",
    "bool",
    "byte",
    "atomicint",
    "complex",
    "size_t",
    "ssize_t",
];

/// Callables the compiler special-cases into dedicated opcodes, so they never
/// reach the runtime function-dispatch tables (`is_builtin_function` /
/// `EVAL_KNOWN_ROUTINE_NAMES` don't list them all).
const COMPILER_SPECIAL_CALL_NAMES: &[&str] = &[
    "cas",
    "atomic-assign",
    "atomic-fetch",
    "atomic-fetch-add",
    "atomic-add-fetch",
    "atomic-fetch-sub",
    "atomic-sub-fetch",
    "atomic-fetch-inc",
    "atomic-inc-fetch",
    "atomic-fetch-dec",
    "atomic-dec-fetch",
    "done",
    "temp",
];

/// Phaser names, offered as suggestions for a lowercase typo (`begin` →
/// "Did you mean 'BEGIN'?", matching rakudo).
pub(crate) const PHASER_SUGGESTION_NAMES: &[&str] = &[
    "BEGIN", "CHECK", "INIT", "END", "ENTER", "LEAVE", "KEEP", "UNDO", "FIRST", "NEXT", "LAST",
    "PRE", "POST", "QUIT", "CLOSE",
];

#[derive(Default)]
struct Scan {
    declared: HashSet<String>,
    calls: Vec<(String, i64)>,
    line: i64,
    /// The unit imports names the walker cannot see (use/require/...): skip
    /// the whole check rather than risk a false positive.
    bail: bool,
}

impl Scan {
    fn declare(&mut self, name: &str) {
        let bare = name.strip_prefix('\\').unwrap_or(name);
        let bare = bare
            .strip_prefix(['$', '@', '%', '&'])
            .unwrap_or(bare)
            .trim_start_matches(['!', '.', '*', '^', ':']);
        if bare.is_empty() {
            return;
        }
        self.declared.insert(bare.to_string());
        // `sub term:<x> {...}` declares the bare term `x`.
        if let Some(inner) = bare
            .strip_prefix("term:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.declared.insert(inner.to_string());
        }
    }

    fn record_call(&mut self, name: &str) {
        if self.bail {
            return;
        }
        // `require`/`import` in call form pull in names at runtime that the
        // walker cannot see — give up on the whole unit.
        if matches!(name, "require" | "import" | "need" | "use" | "EVALFILE") {
            self.bail = true;
            return;
        }
        let Some(first) = name.chars().next() else {
            return;
        };
        // Only plain lowercase bareword calls are checked: uppercase names are
        // type coercions (a different error class with many more legitimate
        // sources), qualified/adverbed names carry `:`/`::`, and `__`-names
        // are compiler-synthesized.
        if !first.is_ascii_lowercase() || name.contains(':') || name.starts_with("__") {
            return;
        }
        self.calls.push((name.to_string(), self.line));
    }
}

fn walk_params(params: &[String], defs: &[ParamDef], scan: &mut Scan) {
    for p in params {
        scan.declare(p);
    }
    for d in defs {
        walk_param_def(d, scan);
    }
}

fn walk_param_def(def: &ParamDef, scan: &mut Scan) {
    scan.declare(&def.name);
    if let Some(e) = &def.default {
        walk_expr(e, scan);
    }
    if let Some(e) = &def.where_constraint {
        walk_expr(e, scan);
    }
    for nested in def
        .sub_signature
        .iter()
        .chain(def.outer_sub_signature.iter())
    {
        for d in nested {
            walk_param_def(d, scan);
        }
    }
    if let Some((defs, _)) = &def.code_signature {
        for d in defs {
            walk_param_def(d, scan);
        }
    }
}

fn walk_stmts(stmts: &[Stmt], scan: &mut Scan) {
    for s in stmts {
        if scan.bail {
            return;
        }
        walk_stmt(s, scan);
    }
}

fn walk_stmt(stmt: &Stmt, scan: &mut Scan) {
    match stmt {
        Stmt::SetLine(n) => scan.line = *n,
        Stmt::Use { .. } | Stmt::No { .. } | Stmt::Need { .. } | Stmt::Import { .. } => {
            scan.bail = true;
        }
        Stmt::VarDecl {
            name,
            expr,
            where_constraint,
            custom_traits,
            ..
        } => {
            scan.declare(name);
            walk_expr(expr, scan);
            if let Some(e) = where_constraint {
                walk_expr(e, scan);
            }
            for (_, arg) in custom_traits {
                if let Some(e) = arg {
                    walk_expr(e, scan);
                }
            }
        }
        Stmt::Assign { name, expr, .. } => {
            // Not a declaration, but suppressing calls to an assigned name is
            // the safe (false-negative) direction.
            scan.declare(name);
            walk_expr(expr, scan);
        }
        Stmt::SubDecl {
            name,
            name_expr,
            params,
            param_defs,
            signature_alternates,
            body,
            ..
        } => {
            if name_expr.is_some() {
                // Dynamically-named sub: the declared name is unknowable.
                scan.bail = true;
                return;
            }
            scan.declare(&name.resolve());
            walk_params(params, param_defs, scan);
            for (alt_params, alt_defs) in signature_alternates {
                walk_params(alt_params, alt_defs, scan);
            }
            walk_stmts(body, scan);
        }
        Stmt::MethodDecl {
            name,
            params,
            param_defs,
            body,
            ..
        } => {
            scan.declare(&name.resolve());
            walk_params(params, param_defs, scan);
            walk_stmts(body, scan);
        }
        Stmt::TokenDecl {
            name,
            params,
            param_defs,
            body,
            ..
        }
        | Stmt::RuleDecl {
            name,
            params,
            param_defs,
            body,
            ..
        }
        | Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            body,
            ..
        } => {
            scan.declare(&name.resolve());
            walk_params(params, param_defs, scan);
            walk_stmts(body, scan);
        }
        Stmt::ProtoToken { name } => scan.declare(&name.resolve()),
        Stmt::Package { name, body, .. } => {
            scan.declare(&name.resolve());
            walk_stmts(body, scan);
        }
        Stmt::EnumDecl { name, variants, .. } => {
            scan.declare(&name.resolve());
            for (vname, vexpr) in variants {
                scan.declare(vname);
                if let Some(e) = vexpr {
                    walk_expr(e, scan);
                }
            }
        }
        Stmt::ClassDecl { name, body, .. } | Stmt::AugmentClass { name, body, .. } => {
            scan.declare(&name.resolve());
            walk_stmts(body, scan);
        }
        Stmt::RoleDecl { name, body, .. } => {
            scan.declare(&name.resolve());
            walk_stmts(body, scan);
        }
        Stmt::SubsetDecl {
            name, predicate, ..
        } => {
            scan.declare(&name.resolve());
            if let Some(e) = predicate {
                walk_expr(e, scan);
            }
        }
        Stmt::HasDecl {
            default: Some(e), ..
        } => walk_expr(e, scan),
        Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Goto(e) | Stmt::Take(e, _) => {
            walk_expr(e, scan)
        }
        Stmt::Expr(e) => walk_expr(e, scan),
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                walk_expr(e, scan);
            }
        }
        Stmt::Call { name, args } => {
            // An explicit-invocant call (`foo($obj: ...)`) is a method call.
            if !args.iter().any(|a| matches!(a, CallArg::Invocant(_))) {
                scan.record_call(&name.resolve());
            }
            for a in args {
                walk_call_arg(a, scan);
            }
        }
        Stmt::Subtest { name, body } => {
            walk_expr(name, scan);
            walk_stmts(body, scan);
        }
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::React { body } => walk_stmts(body, scan),
        Stmt::Phaser { body, .. } => walk_stmts(body, scan),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var,
        } => {
            walk_expr(cond, scan);
            if let Some(v) = binding_var {
                scan.declare(v);
            }
            walk_stmts(then_branch, scan);
            walk_stmts(else_branch, scan);
        }
        Stmt::While { cond, body, .. } => {
            walk_expr(cond, scan);
            walk_stmts(body, scan);
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                walk_stmt(init, scan);
            }
            if let Some(c) = cond {
                walk_expr(c, scan);
            }
            if let Some(s) = step {
                walk_expr(s, scan);
            }
            walk_stmts(body, scan);
        }
        Stmt::For {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body,
            ..
        } => {
            walk_expr(iterable, scan);
            if let Some(p) = param {
                scan.declare(p);
            }
            if let Some(d) = param_def.as_ref().as_ref() {
                walk_param_def(d, scan);
            }
            walk_params(params, params_def, scan);
            walk_stmts(body, scan);
        }
        Stmt::Given { topic, body } => {
            walk_expr(topic, scan);
            walk_stmts(body, scan);
        }
        Stmt::When { cond, body } => {
            walk_expr(cond, scan);
            walk_stmts(body, scan);
        }
        Stmt::Whenever {
            supply,
            param,
            body,
        } => {
            walk_expr(supply, scan);
            if let Some(p) = param {
                scan.declare(p);
            }
            walk_stmts(body, scan);
        }
        Stmt::Label { stmt, .. } => walk_stmt(stmt, scan),
        Stmt::Let { index, value, .. } => {
            if let Some(e) = index {
                walk_expr(e, scan);
            }
            if let Some(e) = value {
                walk_expr(e, scan);
            }
        }
        Stmt::TempMethodAssign {
            method_args, value, ..
        } => {
            for e in method_args {
                walk_expr(e, scan);
            }
            walk_expr(value, scan);
        }
        _ => {}
    }
}

fn walk_call_arg(arg: &CallArg, scan: &mut Scan) {
    match arg {
        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => walk_expr(e, scan),
        CallArg::Named { value, .. } => {
            if let Some(v) = value {
                walk_expr(v, scan);
            }
        }
    }
}

fn walk_expr(expr: &Expr, scan: &mut Scan) {
    match expr {
        Expr::Call { name, args } => {
            scan.record_call(&name.resolve());
            for a in args {
                walk_expr(a, scan);
            }
        }
        Expr::AssignExpr { name, expr, .. } => {
            scan.declare(name);
            walk_expr(expr, scan);
        }
        Expr::Grouped(e)
        | Expr::ZenSlice(e)
        | Expr::Eager(e)
        | Expr::Itemize(e)
        | Expr::DeitemizeForBind(e)
        | Expr::PositionalPair(e)
        | Expr::IndirectTypeLookup(e) => walk_expr(e, scan),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => walk_expr(expr, scan),
        Expr::Reduction { expr, .. } => walk_expr(expr, scan),
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            walk_expr(left, scan);
            walk_expr(right, scan);
        }
        Expr::InfixFunc { left, right, .. } => {
            walk_expr(left, scan);
            for r in right {
                walk_expr(r, scan);
            }
        }
        Expr::Feed { source, sink, .. } => {
            walk_expr(source, scan);
            walk_expr(sink, scan);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            walk_expr(cond, scan);
            walk_expr(then_expr, scan);
            walk_expr(else_expr, scan);
        }
        Expr::MethodCall { target, args, .. }
        | Expr::HyperMethodCall { target, args, .. }
        | Expr::CallOn { target, args } => {
            walk_expr(target, scan);
            for a in args {
                walk_expr(a, scan);
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
            walk_expr(target, scan);
            walk_expr(name_expr, scan);
            for a in args {
                walk_expr(a, scan);
            }
        }
        Expr::Index { target, index, .. } => {
            walk_expr(target, scan);
            walk_expr(index, scan);
        }
        Expr::MultiDimIndex { target, dimensions } => {
            walk_expr(target, scan);
            for d in dimensions {
                walk_expr(d, scan);
            }
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
        } => {
            walk_expr(target, scan);
            for d in dimensions {
                walk_expr(d, scan);
            }
            walk_expr(value, scan);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            walk_expr(target, scan);
            walk_expr(index, scan);
            walk_expr(value, scan);
        }
        Expr::Exists { target, arg, .. } => {
            walk_expr(target, scan);
            if let Some(a) = arg {
                walk_expr(a, scan);
            }
        }
        Expr::SymbolicDeref { expr, .. } => walk_expr(expr, scan),
        Expr::SymbolicDerefAssign { expr, value, .. }
        | Expr::IndirectTypeLookupAssign { expr, value } => {
            walk_expr(expr, scan);
            walk_expr(value, scan);
        }
        Expr::IndirectCodeLookup { package, .. } => walk_expr(package, scan),
        Expr::HyperSlice { target, .. } => walk_expr(target, scan),
        Expr::ArrayLiteral(items)
        | Expr::BracketArray(items, _)
        | Expr::CaptureLiteral(items)
        | Expr::StringInterpolation(items) => {
            for it in items {
                walk_expr(it, scan);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(v) = v {
                    walk_expr(v, scan);
                }
            }
        }
        Expr::Block(body)
        | Expr::Gather(body)
        | Expr::DoBlock { body, .. }
        | Expr::Once { body }
        | Expr::PhaserExpr { body, .. }
        | Expr::AnonSub { body, .. } => walk_stmts(body, scan),
        Expr::AnonSubParams {
            params,
            param_defs,
            body,
            ..
        } => {
            walk_params(params, param_defs, scan);
            walk_stmts(body, scan);
        }
        Expr::Lambda { param, body, .. } => {
            scan.declare(param);
            walk_stmts(body, scan);
        }
        Expr::Try { body, catch } => {
            walk_stmts(body, scan);
            if let Some(c) = catch {
                walk_stmts(c, scan);
            }
        }
        Expr::DoStmt(s) => walk_stmt(s, scan),
        _ => {}
    }
}

impl Interpreter {
    /// Build the X::Undeclared::Symbols error for an undeclared routine call,
    /// rakudo-style: `Undeclared routine:\n    <name> used at line <N>` plus
    /// `. Did you mean '<s>'?` when there are suggestions. Marked as a
    /// compile-time error (ParseGeneric + line) so the CLI renders it as
    /// `===SORRY!=== Error while compiling ...`.
    pub(crate) fn undeclared_routine_error(
        name: &str,
        line: i64,
        suggestions: Vec<String>,
    ) -> RuntimeError {
        let mut msg = format!("Undeclared routine:\n    {} used at line {}", name, line);
        if !suggestions.is_empty() {
            msg.push_str(&format!(". Did you mean '{}'?", suggestions.join("', '")));
        }
        let mut err = RuntimeError::undeclared_routine_symbols(name, msg, suggestions);
        err.set_code(Some(RuntimeErrorCode::ParseGeneric));
        if line > 0 {
            err.set_line(Some(line as usize));
        }
        err
    }

    /// Reject a mainline call to a routine that is declared nowhere in the
    /// unit, *before* execution starts (rakudo's CHECK-time
    /// X::Undeclared::Symbols). See the module doc for the conservativeness
    /// contract; returns Ok(()) whenever the unit imports unseen names.
    pub(crate) fn check_undeclared_routines_mainline(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut scan = Scan {
            line: 1,
            ..Default::default()
        };
        walk_stmts(stmts, &mut scan);
        if scan.bail {
            return Ok(());
        }
        for (name, line) in &scan.calls {
            if scan.declared.contains(name)
                || self.has_function(name)
                || self.has_multi_function(name)
                || self.has_proto(name)
                || Self::is_builtin_function(name)
                || Self::is_test_function_name(name)
                || super::system_eval_names::EVAL_KNOWN_ROUTINE_NAMES.contains(&name.as_str())
                || NATIVE_TYPE_NAMES.contains(&name.as_str())
                || COMPILER_SPECIAL_CALL_NAMES.contains(&name.as_str())
                || crate::parser::is_imported_function(name)
                || self.env().contains_key(&format!("&{}", name))
                || self.env().contains_key(name.as_str())
                || self.get_our_var(name).is_some()
                || self.registry().classes.contains_key(name)
                || self.registry().roles.contains_key(name)
                || self.registry().subsets.contains_key(name)
                || self.registry().enum_types.contains_key(name)
            {
                continue;
            }
            let suggestions = self.suggest_routine_names(name);
            return Err(Self::undeclared_routine_error(name, *line, suggestions));
        }
        Ok(())
    }
}
