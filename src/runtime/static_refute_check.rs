//! CHECK-time refutation of statically typed calls (#18874).
//!
//! Rakudo's optimizer rejects, at CHECK time, a call it can prove will never
//! bind: `sub f(Int $x) {}; f("a")` is the compile-time `X::TypeCheck::Argument`
//! "Calling f(Str) will never work with declared signature (Int $x)", and
//! nothing in the unit runs. mutsu reaches the same verdict when the call site
//! executes (`calls_static_refute`); this pass reaches it before the mainline
//! runs, for the calls it can judge exactly.
//!
//! It judges only what it can know without running anything:
//!
//! - the callee is a plain `sub` declared once in the whole unit, at its top
//!   level: no `multi`, no `proto`, no other declaration of the name at any
//!   depth, and no `&name` anywhere (a rebinding or wrap could replace it);
//! - every argument is a literal (`Int`, `Num`, `Rat`, `Str`, `Bool`, `Nil`)
//!   and none is named;
//! - the refutation is an arity mismatch, a required named parameter with no
//!   named argument, or a literal that fails a core nominal type (or the
//!   `Positional` / `Associative` / `Callable` of an `@` / `%` / `&`
//!   parameter). As in rakudo, a parameter with a default, an `is rw` one, a
//!   `:D`/`:U` smiley and a coercion type are left to the run-time binder,
//!   and a signature with a type capture or a sub-signature is not judged.
//!
//! The error points at the call (`<HERE>`). Its position is found in the text
//! of the innermost statement that holds the call, by matching the calls to
//! the name in that statement against the name's occurrences there; when the
//! counts differ the position cannot be told, and the call is left to the
//! run-time check as before. Exact or absent.

use std::collections::{HashMap, HashSet};

use crate::ast::{CallArg, Expr, ParamDef, SrcSpan, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt, walk_stmts};
use crate::value::{RuntimeError, RuntimeErrorCode, Value, ValueView};

use super::Interpreter;

/// One call to a candidate callee, with literal arguments only.
struct Site<'ast> {
    name: &'ast str,
    args: Vec<Value>,
    /// The innermost statement holding the call.
    span: SrcSpan,
}

/// What one walk over the unit collects.
#[derive(Default)]
struct Survey<'ast> {
    /// How often each name is declared, at any depth (a `sub`, a method, a
    /// package or type, a token).
    decls: HashMap<String, usize>,
    /// Names that appear as `&name` or as a declared `&name` variable.
    code_vars: HashSet<String>,
    /// Names declared as a type anywhere in the unit.
    type_decls: HashSet<String>,
    /// Every by-name call whose arguments are all literals, in source order.
    sites: Vec<Site<'ast>>,
    /// The span of the statement the walk is in.
    span: SrcSpan,
}

impl<'ast> Visit<'ast> for Survey<'ast> {
    fn visit_stmts(&mut self, body: &'ast [Stmt]) {
        let outer = self.span;
        walk_stmts(self, body);
        self.span = outer;
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::SetLine(_, span) if *span != SrcSpan::NONE => self.span = *span,
            Stmt::Call { name, args, .. } => {
                if let Some(args) = literal_call_args(args) {
                    self.sites.push(Site {
                        name: name.as_str(),
                        args,
                        span: self.span,
                    });
                }
            }
            _ => {}
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Call { name, args, .. } = expr
            && let Some(args) = literal_expr_args(args)
        {
            self.sites.push(Site {
                name: name.as_str(),
                args,
                span: self.span,
            });
        }
        walk_expr(self, expr);
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match kind {
            NameKind::SubDecl | NameKind::Decl => {
                *self.decls.entry(name.to_string()).or_insert(0) += 1;
                if kind == NameKind::Decl {
                    self.type_decls.insert(name.to_string());
                }
            }
            NameKind::CodeVar | NameKind::VarDecl | NameKind::Param | NameKind::BlockParam
                if kind == NameKind::CodeVar || name.starts_with('&') =>
            {
                self.code_vars
                    .insert(name.trim_start_matches('&').to_string());
            }
            _ => {}
        }
    }
}

/// The literal value of a call argument, when it is one a refutation can be
/// judged on (`None` for anything else, a named argument included).
// Cost: O(1).
fn literal_arg(arg: &Expr) -> Option<Value> {
    match arg {
        Expr::Literal(value)
            if matches!(
                value.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Rat(..)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Nil
            ) =>
        {
            Some(value.clone())
        }
        _ => None,
    }
}

/// Whether `arg` is the line marker the parser adds to a call (not an argument).
// Cost: O(1).
fn is_callsite_marker(arg: &Expr) -> bool {
    matches!(arg, Expr::Binary { left, op: crate::token_kind::TokenKind::FatArrow, .. }
        if matches!(left.as_ref(), Expr::Literal(key)
            if matches!(key.view(), ValueView::Str(key)
                if key.as_str() == crate::parser::TEST_CALLSITE_LINE_KEY)))
}

// Cost: O(a), a = arguments.
fn literal_expr_args(args: &[Expr]) -> Option<Vec<Value>> {
    args.iter()
        .filter(|arg| !is_callsite_marker(arg))
        .map(literal_arg)
        .collect()
}

// Cost: O(a), a = arguments.
fn literal_call_args(args: &[CallArg]) -> Option<Vec<Value>> {
    args.iter()
        .filter_map(|arg| match arg {
            CallArg::Named { name, .. } if name == crate::parser::TEST_CALLSITE_LINE_KEY => None,
            CallArg::Positional(expr) if is_callsite_marker(expr) => None,
            CallArg::Positional(expr) => Some(literal_arg(expr)),
            _ => Some(None),
        })
        .collect()
}

/// The parameter type a positional literal must match, or `None` when the
/// binder, not the signature, decides (a default, `is rw`, a smiley, a
/// coercion or parametric type, a type the unit declares itself).
// Cost: O(t), t = length of the type name.
fn judged_type<'a>(param: &'a ParamDef, user_types: &HashSet<String>) -> Option<&'a str> {
    if param.default.is_some() || param.traits.iter().any(|t| t == "rw") {
        return None;
    }
    let declared = param.type_constraint.as_deref();
    let ty = match (declared, param.name.chars().next()) {
        (Some(ty), _) => ty,
        (None, Some('@')) => "Positional",
        (None, Some('%')) => "Associative",
        (None, Some('&')) => "Callable",
        (None, _) => return None,
    };
    if ty.contains([':', '(', '[', '{'])
        || user_types.contains(ty)
        || !crate::known_types::is_known_type_constraint(ty)
    {
        return None;
    }
    Some(ty)
}

impl Interpreter {
    /// The CHECK-time refutation of the unit's statically typed calls, as the
    /// compile error rakudo raises for the first one; `Ok` when none can be
    /// told (see the module docs). `source` is the unit's text, which the
    /// statement spans index.
    // Cost: O(n + c * (p + s)), n = size of the unit, c = judged calls,
    // p = parameters of the callee, s = length of the call's statement.
    pub(crate) fn check_static_refutations(
        &mut self,
        stmts: &[Stmt],
        source: &str,
    ) -> Result<(), RuntimeError> {
        let mut survey = Survey::default();
        survey.visit_stmts(stmts);
        if survey.sites.is_empty() {
            return Ok(());
        }
        let callees: HashMap<&str, &[ParamDef]> = stmts
            .iter()
            .filter_map(|stmt| match stmt {
                Stmt::SubDecl {
                    name,
                    name_expr: None,
                    param_defs,
                    signature_alternates,
                    multi: false,
                    ..
                } if signature_alternates.is_empty() => Some((name.as_str(), param_defs.as_slice())),
                _ => None,
            })
            .filter(|(name, _)| {
                survey.decls.get(*name) == Some(&1) && !survey.code_vars.contains(*name)
            })
            .collect();
        for (index, site) in survey.sites.iter().enumerate() {
            let Some(params) = callees.get(site.name) else {
                continue;
            };
            if !self.refutes(params, &site.args, &survey.type_decls) {
                continue;
            }
            let Some(at) = call_offset(&survey.sites, index, source) else {
                continue;
            };
            return Err(refutation_error(site.name, params, &site.args, source, at));
        }
        Ok(())
    }

    /// Whether no binding of `args` to `params` can ever succeed, by the
    /// rules rakudo's optimizer applies (see the module docs).
    // Cost: O(p + a * t), p = parameters, a = arguments, t = one type match.
    fn refutes(&mut self, params: &[ParamDef], args: &[Value], user_types: &HashSet<String>) -> bool {
        let unjudged = |p: &ParamDef| {
            p.type_capture.is_some()
                || p.target == crate::ast::ParamTarget::TypeCapture
                || p.sub_signature.is_some()
                || p.outer_sub_signature.is_some()
                || p.literal_value.is_some()
                || p.is_invocant
        };
        if params.iter().any(unjudged) {
            return false;
        }
        if params.iter().any(|p| p.named && p.required) {
            return true;
        }
        let positional: Vec<&ParamDef> = params
            .iter()
            .filter(|p| !p.named && !p.double_slurpy)
            .collect();
        let slurpy = positional.iter().any(|p| p.slurpy);
        let fixed: Vec<&ParamDef> = positional.iter().copied().filter(|p| !p.slurpy).collect();
        let required = fixed
            .iter()
            .filter(|p| !p.optional_marker && p.default.is_none())
            .count();
        if args.len() < required || (!slurpy && args.len() > fixed.len()) {
            return true;
        }
        fixed.iter().zip(args).any(|(param, arg)| {
            judged_type(param, user_types).is_some_and(|ty| !self.type_matches_value(ty, arg))
        })
    }
}

/// The byte offset of the `index`-th site's call in `source`: the k-th
/// occurrence of its name in its statement's text, when the statement holds
/// exactly as many calls to the name as the text has occurrences.
// Cost: O(c + s), c = sites, s = length of the statement.
fn call_offset(sites: &[Site<'_>], index: usize, source: &str) -> Option<usize> {
    let site = &sites[index];
    let span = site.span;
    if span == SrcSpan::NONE {
        return None;
    }
    let inside = |other: &Site<'_>| {
        other.name == site.name && other.span.from >= span.from && other.span.to <= span.to
    };
    let rank = sites[..index].iter().filter(|other| inside(other)).count();
    let total = sites.iter().filter(|other| inside(other)).count();
    let text = source.get(span.from as usize..span.to as usize)?;
    let occurrences = word_occurrences(text, site.name);
    (occurrences.len() == total).then(|| span.from as usize + occurrences[rank])
}

/// The byte offsets in `text` where `name` stands as a whole routine name.
// Cost: O(s), s = length of `text`.
fn word_occurrences(text: &str, name: &str) -> Vec<usize> {
    let word_char = |c: char| c.is_alphanumeric() || c == '_';
    text.match_indices(name)
        .map(|(at, _)| at)
        .filter(|&at| {
            let before = text[..at].chars().next_back();
            let after = text[at + name.len()..].chars().next();
            let joins_after = |c: char| {
                word_char(c)
                    || (matches!(c, '-' | '\'')
                        && text[at + name.len() + 1..]
                            .chars()
                            .next()
                            .is_some_and(char::is_alphabetic))
            };
            !before.is_some_and(|c| word_char(c) || matches!(c, '$' | '@' | '%' | '&' | '.' | '!' | '-' | '\'' | ':'))
                && !after.is_some_and(joins_after)
        })
        .collect()
}

/// The compile-time `X::TypeCheck::Argument` for the call at byte `at`.
// Cost: O(p + a + s), p = parameters, a = arguments, s = length of `source`.
fn refutation_error(
    name: &str,
    params: &[ParamDef],
    args: &[Value],
    source: &str,
    at: usize,
) -> RuntimeError {
    let arg_types = Interpreter::arg_type_names(args);
    let signature = Interpreter::build_signature_string(params);
    let message = format!(
        "Calling {name}({}) will never work with declared signature {signature}",
        arg_types.join(", ")
    );
    let line = 1 + source[..at].matches('\n').count();
    let column = 1 + source[..at].rsplit('\n').next().map_or(0, |l| l.chars().count());
    let pos = source[..at].chars().count();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("objname".to_string(), Value::str(name.to_string()));
    attrs.insert("signature".to_string(), Value::str(signature));
    attrs.insert(
        "arguments".to_string(),
        Value::array(arg_types.into_iter().map(Value::str).collect()),
    );
    attrs.insert("line".to_string(), Value::int(line as i64));
    attrs.insert("column".to_string(), Value::int(column as i64));
    attrs.insert("pos".to_string(), Value::int(pos as i64));
    let mut err = RuntimeError::new(message);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    err.set_line(Some(line));
    err.set_column(Some(column));
    err.set_parse_pos(pos);
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern("X::TypeCheck::Argument"),
        attrs,
    )));
    err
}
