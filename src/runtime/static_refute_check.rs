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
//!   level, or the unit's top-level `multi` candidates (with at most one
//!   `proto`): no other declaration of the name at any depth, and no `&name`
//!   anywhere (a rebinding or wrap could replace it). A multi is judged only
//!   when nothing else can add candidates: its name is no core or `Test`
//!   routine, and the unit imports no module. It is refuted when every
//!   candidate is, by the dispatcher's rules: a default, a `:D` smiley and an
//!   optional named parameter do not save a candidate;
//! - every argument is a literal (`Int`, `Num`, `Rat`, `Str`, `Bool`, `Nil`)
//!   and none is named;
//! - the refutation is an arity mismatch, a required named parameter with no
//!   named argument, or a literal that fails a core nominal type (or the
//!   `Positional` / `Associative` / `Callable` of an `@` / `%` / `&`
//!   parameter). As in rakudo, a parameter with a default, an `is rw` one, a
//!   `:D`/`:U` smiley and a coercion type are left to the run-time binder.
//!   A native type (`int`, `uint8`) is left to it too. A signature with an
//!   optional named parameter is not judged at all, as in rakudo, and neither
//!   is a routine written without a signature (its `@_` / placeholders are
//!   not in its parameter list) or one with a type capture or a sub-signature.
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
    /// Whether the unit imports names from a module (`use`, `need`,
    /// `import`), whose `multi` candidates it cannot see. `Test` exports only
    /// its own functions, which are never judged.
    imports: bool,
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
            NameKind::Module
                if name != "Test"
                    && !super::undeclared_routines::module_imports_no_names(name) =>
            {
                self.imports = true;
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

/// Which of rakudo's two static checks judges a callee: an `only` sub's, or a
/// multi's, which tries every candidate as the dispatcher would.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Rules {
    Only,
    Multi,
}

/// The parameter type a positional literal must match, or `None` when the
/// binder, not the signature, decides (`is rw`, a coercion or parametric
/// type, a type the unit declares itself; for an `only` sub also a default
/// and a smiley, which a multi candidate's dispatch still judges: a literal
/// is defined, so `Int:D` is `Int` for it).
// Cost: O(t), t = length of the type name.
fn judged_type<'a>(
    param: &'a ParamDef,
    user_types: &HashSet<String>,
    rules: Rules,
) -> Option<&'a str> {
    if param.traits.iter().any(|t| t == "rw") || (rules == Rules::Only && param.default.is_some())
    {
        return None;
    }
    let declared = param.type_constraint.as_deref().map(|ty| match rules {
        Rules::Multi => ty.strip_suffix(":D").unwrap_or(ty),
        Rules::Only => ty,
    });
    let ty = match (declared, param.name.chars().next()) {
        (Some(ty), _) => ty,
        (None, Some('@')) => "Positional",
        (None, Some('%')) => "Associative",
        (None, Some('&')) => "Callable",
        (None, _) => return None,
    };
    // A native type (`int`, `uint8`) unboxes at run time: rakudo refutes
    // some of its mismatches statically and not others (`int` takes a Rat
    // until the binder truncates it), so the binder decides.
    if ty.contains([':', '(', '[', '{'])
        || crate::native_types::is_native_type_name(ty)
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
        let callees = callees(stmts, &survey);
        for (index, site) in survey.sites.iter().enumerate() {
            let Some((rules, candidates)) = callees.get(site.name) else {
                continue;
            };
            let refuted = candidates
                .iter()
                .all(|params| self.refutes(params, &site.args, &survey.type_decls, *rules));
            if !refuted {
                continue;
            }
            let Some(at) = call_offset(&survey.sites, index, source) else {
                continue;
            };
            return Err(refutation_error(site.name, *rules, candidates, &site.args, source, at));
        }
        Ok(())
    }

    /// Whether no binding of `args` to `params` can ever succeed, by the
    /// rules rakudo's optimizer applies (see the module docs).
    // Cost: O(p + a * t), p = parameters, a = arguments, t = one type match.
    fn refutes(
        &mut self,
        params: &[ParamDef],
        args: &[Value],
        user_types: &HashSet<String>,
        rules: Rules,
    ) -> bool {
        let unjudged = |p: &ParamDef| {
            p.type_capture.is_some()
                || p.target == crate::ast::ParamTarget::TypeCapture
                || p.sub_signature.is_some()
                || p.outer_sub_signature.is_some()
                || p.literal_value.is_some()
                || p.is_invocant
        };
        // A routine written without a signature takes what its body uses
        // (`@_`, `%_`, placeholders), which its parameter list does not show.
        // rakudo's optimizer does not judge a signature with an optional
        // named parameter at all (`sub f($n, :$a) {}; f(1, 2)` is the
        // run-time "Too many positionals passed").
        // A multi candidate's dispatch judges it all the same.
        let optional_named = |p: &ParamDef| rules == Rules::Only && p.named && !p.required;
        if params.is_empty() || params.iter().any(|p| unjudged(p) || optional_named(p)) {
            return false;
        }
        if params.iter().any(|p| p.named && p.required) {
            return true;
        }
        let positional: Vec<&ParamDef> = params
            .iter()
            // `*%h` takes named arguments only (`double_slurpy` is `**@a`).
            .filter(|p| !p.named && !(p.slurpy && p.name.starts_with('%')))
            .collect();
        // `*@a`, `+@a` and `|c` take any number of positionals.
        let takes_many = |p: &ParamDef| p.slurpy || p.onearg;
        let slurpy = positional.iter().any(|p| takes_many(p));
        let fixed: Vec<&ParamDef> = positional
            .iter()
            .copied()
            .filter(|p| !takes_many(p))
            .collect();
        let required = fixed
            .iter()
            .filter(|p| !p.optional_marker && p.default.is_none())
            .count();
        if args.len() < required || (!slurpy && args.len() > fixed.len()) {
            return true;
        }
        fixed.iter().zip(args).any(|(param, arg)| {
            judged_type(param, user_types, rules)
                .is_some_and(|ty| !self.type_matches_value(ty, arg))
        })
    }
}

/// The callees the pass judges, by name: an `only` sub declared once at the
/// unit's top level, or the unit's top-level `multi` candidates (with their
/// `proto`, if any) when the name is declared nowhere else; never a name
/// written as `&name`.
// Cost: O(s), s = top-level statements.
fn callees<'ast>(
    stmts: &'ast [Stmt],
    survey: &Survey<'_>,
) -> HashMap<&'ast str, (Rules, Vec<&'ast [ParamDef]>)> {
    let mut found: HashMap<&str, (Rules, Vec<&[ParamDef]>)> = HashMap::new();
    let mut protos: HashMap<&str, usize> = HashMap::new();
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl {
                name,
                name_expr: None,
                param_defs,
                signature_alternates,
                multi,
                ..
            } if signature_alternates.is_empty() => {
                let rules = if *multi { Rules::Multi } else { Rules::Only };
                let entry = found.entry(name.as_str()).or_insert((rules, Vec::new()));
                if entry.0 != rules {
                    entry.1.clear();
                    entry.0 = Rules::Only;
                    continue;
                }
                entry.1.push(param_defs.as_slice());
            }
            Stmt::ProtoDecl { name, .. } => *protos.entry(name.as_str()).or_insert(0) += 1,
            _ => {}
        }
    }
    found.retain(|name, (rules, candidates)| {
        let protos = protos.get(name).copied().unwrap_or(0);
        let declared = survey.decls.get(*name).copied().unwrap_or(0);
        // A `multi` adds to the candidates a core routine of the same name
        // already has (`multi min(Int $a)`), which the unit does not show.
        let whole = match rules {
            Rules::Only => candidates.len() == 1 && protos == 0,
            Rules::Multi => {
                !candidates.is_empty()
                    && protos <= 1
                    && !survey.imports
                    && !Interpreter::is_builtin_function(name)
                    && !Interpreter::is_test_function_name(name)
            }
        };
        whole && declared == candidates.len() + protos && !survey.code_vars.contains(*name)
    });
    found
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
    rules: Rules,
    candidates: &[&[ParamDef]],
    args: &[Value],
    source: &str,
    at: usize,
) -> RuntimeError {
    let arg_types = Interpreter::arg_type_names(args);
    let signatures: Vec<String> = candidates
        .iter()
        .map(|params| Interpreter::build_signature_string(params))
        .collect();
    let call = format!("Calling {name}({})", arg_types.join(", "));
    let message = match rules {
        Rules::Only => format!("{call} will never work with declared signature {}", signatures[0]),
        Rules::Multi => format!(
            "{call} will never work with any of these multi signatures:\n    {}",
            signatures.join("\n    ")
        ),
    };
    let signature = signatures.join("\n");
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
