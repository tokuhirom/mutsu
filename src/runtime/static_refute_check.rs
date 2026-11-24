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
//!   variable or parameter anywhere (it would shadow the routine; a plain
//!   `&name` reference or `.wrap` does not stop rakudo refuting the call). A multi is judged only
//!   when nothing else can add candidates: its name is no core or `Test`
//!   routine, and the unit imports no module. It is refuted when every
//!   candidate is, by the dispatcher's rules: a default, a `:D` smiley and an
//!   optional named parameter do not save a candidate;
//! - every argument is a literal (`Int`, `Num`, `Rat`, `Str`, `Bool`, `Nil`),
//!   a core type object (`Str`; a supertype of the parameter's type, such
//!   as `Cool` for `Int`, may still bind and is not refuted) or a scalar
//!   variable or parameter declared with a core type (`my Str $s`,
//!   `sub g(Str $s)`, at any depth), which stands for its type object when
//!   its name is declared nowhere else in the unit; none is named,
//!   and none is written as a word quote (`<a>`, which rakudo does not judge);
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

use crate::ast::{Expr, ParamDef, SrcSpan, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_param, walk_stmt, walk_stmts};
use crate::value::{RuntimeError, Value, ValueView};

use super::Interpreter;

mod args;
mod report;
use args::{static_arg, static_call_args, static_expr_args, typed_param, typed_var_decl, typed_vars};
use report::{call_offset, refutation_error, written_with_word_quote};

/// One call whose arguments are all static-shaped (see [`static_arg`]).
struct Site<'ast> {
    name: &'ast str,
    args: Vec<&'ast Expr>,
    /// The innermost statement holding the call.
    span: SrcSpan,
}

/// What one walk over the unit collects.
#[derive(Default)]
struct Survey<'ast> {
    /// How often each name is declared, at any depth (a `sub`, a method, a
    /// package or type, a token).
    decls: HashMap<String, usize>,
    /// Names declared as a `&name` variable or parameter, which shadows a
    /// routine of the name where it is in scope. A plain `&name` reference
    /// (taken, passed, `.wrap`ped) does not: rakudo refutes the call all the
    /// same.
    code_vars: HashSet<String>,
    /// Names written as a plain `&name` reference anywhere. A `multi` written
    /// so is not judged: the reference can add candidates
    /// (`&p.add_dispatchee(&other)` at BEGIN time).
    code_refs: HashSet<String>,
    /// Whether the unit imports names from a module (`use`, `need`,
    /// `import`), whose `multi` candidates it cannot see. `Test` exports only
    /// its own functions, which are never judged.
    imports: bool,
    /// Names declared as a type anywhere in the unit.
    type_decls: HashSet<String>,
    /// How often each variable name (without its sigil) is declared, at any
    /// depth: a `my`/`our`/`state`, a parameter, a block or loop parameter.
    var_decls: HashMap<String, usize>,
    /// Every typed scalar declaration or parameter, at any depth, as its
    /// name and type (see `typed_vars`).
    typed: Vec<(&'ast str, &'ast str)>,
    /// Every by-name call whose arguments are all literals, in source order.
    sites: Vec<Site<'ast>>,
    /// The span of the statement the walk is in.
    span: SrcSpan,
    /// The flat parameter names of the declaration being walked, which the
    /// walk reports before the same parameters' `ParamDef`s: a parameter
    /// is one declaration, counted once. Cleared at every statement and
    /// expression, so a name is only ever matched within its declaration.
    flat: Vec<String>,
}

impl<'ast> Visit<'ast> for Survey<'ast> {
    fn visit_stmts(&mut self, body: &'ast [Stmt]) {
        self.flat.clear();
        let outer = self.span;
        walk_stmts(self, body);
        self.span = outer;
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        self.flat.clear();
        match stmt {
            Stmt::SetLine(_, span) if *span != SrcSpan::NONE => self.span = *span,
            Stmt::Call { name, args, .. } => {
                if let Some(args) = static_call_args(args) {
                    self.sites.push(Site {
                        name: name.as_str(),
                        args,
                        span: self.span,
                    });
                }
            }
            _ => {}
        }
        if let Some(typed) = typed_var_decl(stmt) {
            self.typed.push(typed);
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.flat.clear();
        if let Expr::Call { name, args, .. } = expr
            && let Some(args) = static_expr_args(args)
        {
            self.sites.push(Site {
                name: name.as_str(),
                args,
                span: self.span,
            });
        }
        walk_expr(self, expr);
    }

    fn visit_param(&mut self, param: &'ast ParamDef) {
        self.typed.extend(typed_param(param));
        walk_param(self, param);
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
            NameKind::CodeVar => {
                self.code_refs.insert(name.trim_start_matches('&').to_string());
            }
            NameKind::VarDecl | NameKind::Param | NameKind::BlockParam => {
                if let Some(code) = name.strip_prefix('&') {
                    self.code_vars.insert(code.to_string());
                }
                let bare = name.trim_start_matches(['$', '@', '%', '&']);
                if kind == NameKind::Param
                    && let Some(at) = self.flat.iter().position(|flat| flat == bare)
                {
                    self.flat.swap_remove(at);
                    return;
                }
                if kind == NameKind::BlockParam {
                    self.flat.push(bare.to_string());
                }
                *self.var_decls.entry(bare.to_string()).or_insert(0) += 1;
            }
            // `has $x` is read as `$x` in the class's methods.
            NameKind::Attribute => {
                let bare = name.trim_start_matches(['$', '@', '%', '&', '.', '!']);
                *self.var_decls.entry(bare.to_string()).or_insert(0) += 1;
            }
            _ => {}
        }
    }
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
        let typed_vars = typed_vars(&survey);
        for (index, site) in survey.sites.iter().enumerate() {
            let Some((rules, candidates)) = callees.get(site.name) else {
                continue;
            };
            let Some(args) = site
                .args
                .iter()
                .map(|arg| static_arg(arg, &typed_vars))
                .collect::<Option<Vec<Value>>>()
            else {
                continue;
            };
            // A type object the unit redeclares (a class, a sub of the name)
            // is not the core one.
            let redeclared = args.iter().any(|arg| {
                matches!(arg.view(), ValueView::Package(name)
                    if survey.decls.contains_key(name.resolve().as_str()))
            });
            if redeclared {
                continue;
            }
            let refuted = candidates
                .iter()
                .all(|params| self.refutes(params, &args, &survey.type_decls, *rules));
            if !refuted {
                continue;
            }
            let Some(at) = call_offset(&survey.sites, index, source) else {
                continue;
            };
            if written_with_word_quote(source, at + site.name.len(), site.span) {
                continue;
            }
            return Err(refutation_error(site.name, *rules, candidates, &args, source, at));
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
        // A type object that is a supertype of its parameter's type (`f(Cool)`
        // for `Int $x`, `f(Mu)`) may still name a value that binds: rakudo then
        // leaves the whole call to the binder (#10944), arity included and
        // whatever the other arguments are.
        let mut mismatched = false;
        for (param, arg) in fixed.iter().zip(args) {
            let Some(ty) = judged_type(param, user_types, rules) else {
                continue;
            };
            if self.type_matches_value(ty, arg) {
                continue;
            }
            if let ValueView::Package(name) = arg.view() {
                let param_type = Value::package(crate::symbol::Symbol::intern(ty));
                if self.type_matches_value(&name.resolve(), &param_type) {
                    return false;
                }
            }
            mismatched = true;
        }
        let required = fixed
            .iter()
            .filter(|p| !p.optional_marker && p.default.is_none())
            .count();
        mismatched || args.len() < required || (!slurpy && args.len() > fixed.len())
    }
}

/// Core traits, which leave the signature as written.
const CORE_TRAITS: &[&str] = &["DEPRECATED", "test-assertion", "implementation-detail"];
const CORE_PARAM_TRAITS: &[&str] = &["copy", "rw", "raw", "readonly"];

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
    let mut traited: HashSet<&str> = HashSet::new();
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl {
                name,
                name_expr: None,
                param_defs,
                signature_alternates,
                multi,
                custom_traits,
                ..
            } if signature_alternates.is_empty() => {
                // A user trait runs at BEGIN time and can rewrite the signature
                // (Deps' `is injected` makes every parameter optional); `__`
                // traits are mutsu's internal markers.
                let core = |t: &String, core: &[&str]| core.contains(&t.as_str());
                if !custom_traits.iter().all(|(t, _)| t.starts_with("__") || core(t, CORE_TRAITS))
                    || !param_defs.iter().flat_map(|p| &p.traits).all(|t| core(t, CORE_PARAM_TRAITS))
                {
                    traited.insert(name.as_str());
                }
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
        if traited.contains(name) {
            return false;
        }
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
                    && !survey.code_refs.contains(*name)
                    && !Interpreter::is_builtin_function(name)
                    && !Interpreter::is_test_function_name(name)
            }
        };
        whole && declared == candidates.len() + protos && !survey.code_vars.contains(*name)
    });
    found
}
