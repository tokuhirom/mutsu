//! The undeclared-variable scan of an `EVAL`'d snippet: a typed AST visitor
//! (ADR-0137) with a stack of lexical scopes.
//!
//! **Declarations** are seen everywhere, in source order. A declaration
//! enters the innermost scope — a `my` initializer already sees the new name
//! (`my $x = $x`), a use *before* an embedded `my` does not
//! (`$foo ~ my $foo`) — and a block, routine, type body or closure opens a
//! scope that ends with it. A condition or loop header (`if my $x = ...`,
//! `loop (my $i = 0; ...)`) declares into the enclosing scope, as in rakudo.
//!
//! **Uses** are judged only in some positions: a statement of the snippet
//! (or of a block, routine or type body inside it) that is an expression,
//! `say`/`print`/`put`/`note` or an assignment, and inside it the variables
//! reached through interpolation, subscripts, method calls and — on an
//! assignment's right-hand side — operators. Rakudo judges every position,
//! but a free variable the snippet does not declare may be a lexical of the
//! *caller's* pad that is declared later in the caller's source (rakudo's
//! pads are static: `{ sub e($s) { EVAL $s }; e('!$y.defined'); my $y }`
//! is fine), and the runtime environment the check consults does not know
//! such names yet. Until it does (#10511), widening the judged positions
//! would turn those into false compile errors, so the walk turns judging
//! off (`Mode::Off`) everywhere else rather than skipping those subtrees.

use super::*;
use crate::ast::ParamDef;
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt, walk_stmts};
use crate::regex_tree::RegexNode;

pub(super) type Undeclared = (&'static str, String, Vec<String>);

/// The first variable used without a declaration in scope, as
/// `(sigil, name, suggestions)`.
// Cost: O(n * d), n = size of the snippet's AST, d = depth of the scope
// stack.
pub(super) fn first_undeclared_var(interp: &Interpreter, stmts: &[Stmt]) -> Option<Undeclared> {
    let mut scan = UndeclaredVars {
        interp,
        scopes: vec![HashSet::new()],
        strict: true,
        judged_stmt: true,
        mode: Mode::Off,
        found: None,
    };
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// How the variables of the expression being walked are judged.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// Not judged (declarations are still collected).
    Off,
    /// Judged through interpolation, subscripts and method calls.
    Plain,
    /// An assignment's right-hand side: also through operators, grouping
    /// and embedded declarations, in source order.
    Ordered,
}

/// The name a declaration (variable, parameter, attribute alias) puts in
/// scope, without its slurpy/capture markers: a scalar without its `$` (the
/// form a `$x` use is looked up by), any other container with its sigil, so
/// `my @barf` does not declare `$barf`.
fn declared_var_name(name: &str) -> &str {
    let name = name.trim_start_matches(['\\', '*', '|', ':']);
    name.strip_prefix('$').unwrap_or(name)
}

/// A parameter name without its sigil, for comparing `where` references.
fn bare_var_name(name: &str) -> &str {
    declared_var_name(name).trim_start_matches(['@', '%', '&'])
}

/// A variable name the check never judges: the topic and placeholder
/// arrays, special and match variables, twigil forms (dynamic, compile-time,
/// placeholder, attribute, pod), package-qualified and compiler-synthesized
/// names.
fn is_exempt(name: &str) -> bool {
    matches!(name, "_" | "/" | "!" | "self" | "¢")
        || name.starts_with(['*', '?', '^', '~', '.', '!', ':', '='])
        || name.contains("::")
        || name.starts_with("CALLER")
        || name.starts_with("DYNAMIC")
        || name.starts_with("__")
        || name.chars().next().is_some_and(|c| c.is_ascii_digit())
        // An extended identifier whose adverb value still awaits BEGIN-time
        // evaluation (`$a:foo«$c»`) does not yet spell the name it will look
        // up -- only the compiler knows the `constant` environment that
        // decides it (`compiler::adverb_interp`).
        || crate::adverb_name::needs_interp(name)
}

struct UndeclaredVars<'a> {
    interp: &'a Interpreter,
    /// Lexical scopes, innermost last, of names in `declared_var_name` form.
    scopes: Vec<HashSet<String>>,
    /// `no strict` is in effect: an assignment declares its target and no
    /// use is judged.
    strict: bool,
    /// The statement being visited is in a judged statement list.
    judged_stmt: bool,
    mode: Mode,
    found: Option<Undeclared>,
}

impl UndeclaredVars<'_> {
    fn declare(&mut self, name: &str) {
        let scope = self.scopes.last_mut().expect("the unit scope");
        scope.insert(declared_var_name(name).to_string());
    }

    fn is_declared(&self, name: &str) -> bool {
        self.scopes.iter().any(|s| s.contains(name))
    }

    /// Runs `f` with the given judging state, restoring the current one.
    fn judging(&mut self, judged_stmt: bool, mode: Mode, f: impl FnOnce(&mut Self)) {
        let saved = (self.judged_stmt, self.mode);
        (self.judged_stmt, self.mode) = (judged_stmt, mode);
        f(self);
        (self.judged_stmt, self.mode) = saved;
    }

    fn in_scope(&mut self, f: impl FnOnce(&mut Self)) {
        self.scopes.push(HashSet::new());
        f(self);
        self.scopes.pop();
    }

    fn all_declared(&self) -> HashSet<String> {
        self.scopes.iter().flatten().cloned().collect()
    }

    fn check_var(&mut self, expr: &Expr) {
        if self.found.is_some() || !self.strict {
            return;
        }
        self.found = match expr {
            Expr::Var(name) => self.undeclared_scalar(name),
            Expr::ArrayVar(name) => self.undeclared_container("@", name),
            Expr::HashVar(name) => self.undeclared_container("%", name),
            _ => None,
        };
    }

    fn undeclared_scalar(&self, name: &str) -> Option<Undeclared> {
        if is_exempt(name) || self.is_declared(name) {
            return None;
        }
        let env = self.interp.env();
        let sigiled = format!("${}", name);
        if env.contains_key(&sigiled) {
            return None;
        }
        // A bare-name environment entry only declares the scalar `$name` if it
        // is an actual variable. A class/sub named `Foo` is stored bare (as a
        // type object), but does NOT declare the scalar `$Foo` -- in Raku
        // `$Foo` is its own (undeclared) symbol.
        if env.contains_key(name)
            && !self.interp.registry().classes.contains_key(name)
            && !self.interp.has_function(name)
        {
            return None;
        }
        let mut suggestions = Interpreter::suggest_declared_vars(&sigiled, &self.all_declared());
        // A same-spelling type is the useful suggestion for a short scalar
        // typo (`$foo` when class `Foo` exists). This mirrors the runtime
        // strict-mode assignment error while retaining the edit-distance
        // filter for genuinely strange variable names.
        if suggestions.is_empty() {
            let mut chars = name.chars();
            if let Some(first) = chars.next() {
                let type_name = format!(
                    "{}{}",
                    first.to_uppercase().collect::<String>(),
                    chars.as_str()
                );
                if self.interp.has_class(&type_name) {
                    suggestions.push(type_name);
                }
            }
        }
        Some(("$", name.to_string(), suggestions))
    }

    fn undeclared_container(&self, sigil: &'static str, name: &str) -> Option<Undeclared> {
        let sigiled = format!("{}{}", sigil, name);
        if is_exempt(name) || self.is_declared(name) || self.is_declared(&sigiled) {
            return None;
        }
        let env = self.interp.env();
        if env.contains_key(&sigiled) || env.contains_key(name) {
            return None;
        }
        let suggestions = Interpreter::suggest_declared_vars(&sigiled, &self.all_declared());
        Some((sigil, name.to_string(), suggestions))
    }

    /// The target of `$x = ...` must be declared (assignment does not declare
    /// under `use strict`); under `no strict` it declares a package variable.
    fn assign_target(&mut self, name: &str, target_is_sigilless: bool) {
        if !self.strict {
            self.declare(name);
            return;
        }
        // A sigil-less target naming an in-scope constant is the term, not an
        // undeclared `$name` (#9962); the store itself reports the immutable
        // value.
        if !self.judged_stmt || target_is_sigilless && self.interp.term_binding(name).is_some() {
            return;
        }
        let lhs = if let Some(base) = name.strip_prefix('@') {
            Expr::ArrayVar(base.to_string())
        } else if let Some(base) = name.strip_prefix('%') {
            Expr::HashVar(base.to_string())
        } else {
            Expr::Var(name.trim_start_matches('$').to_string())
        };
        self.check_var(&lhs);
    }

    /// A routine body's scope: its parameters (declared as the signature is
    /// walked), checked first for a `where` that names a later parameter.
    fn routine(&mut self, param_defs: &[ParamDef], stmt: &Stmt) {
        let judged = self.judged_stmt;
        if judged && let Some(r) = later_param_in_where(param_defs) {
            self.found = Some(r);
            return;
        }
        self.in_scope(|v| v.judging(judged, Mode::Off, |v| walk_stmt(v, stmt)));
    }

    /// A class or role body's scope: its attributes (`has $.x` makes `$!x`
    /// available, the alias form `has $x` also `$x`) and its body lexicals,
    /// which every method of the body sees, wherever they are declared.
    fn type_body(&mut self, body: &[Stmt], stmt: &Stmt) {
        let judged = self.judged_stmt;
        self.in_scope(|v| {
            for s in body {
                match s {
                    Stmt::HasDecl { name, is_alias, .. } => {
                        let attr = name.resolve();
                        // Rakudo's "did you mean" for a bare `$name` that
                        // refers to an attribute suggests `$!name`.
                        v.declare(&format!("!{}", attr));
                        if *is_alias {
                            v.declare(&attr);
                        }
                    }
                    Stmt::VarDecl { name, .. } => v.declare(name),
                    _ => {}
                }
            }
            v.judging(judged, Mode::Off, |v| walk_stmt(v, stmt));
        });
    }
}

impl<'ast> Visit<'ast> for UndeclaredVars<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        let judged = self.judged_stmt;
        match stmt {
            Stmt::No { module, .. } if module == "strict" => self.strict = false,
            Stmt::Use { module, .. } if module == "strict" => self.strict = true,
            // Only uses after it see the new name (`$foo ~ my $foo` is
            // undeclared); its own initializer already does (`my $x = $x`).
            // Embedded in a judged right-hand side, the initializer stays
            // judged.
            Stmt::VarDecl { name, .. } => {
                self.declare(name);
                let mode = if self.mode == Mode::Ordered {
                    Mode::Ordered
                } else {
                    Mode::Off
                };
                self.judging(false, mode, |v| walk_stmt(v, stmt));
            }
            Stmt::Assign {
                name,
                expr,
                target_is_sigilless,
                ..
            } => {
                self.assign_target(name, *target_is_sigilless);
                let mode = if judged { Mode::Ordered } else { Mode::Off };
                self.judging(false, mode, |v| v.visit_expr(expr));
            }
            Stmt::Expr(_) | Stmt::Say(_) | Stmt::Print(_) | Stmt::Put(_) | Stmt::Note(_) => {
                let mode = if judged { Mode::Plain } else { Mode::Off };
                self.judging(false, mode, |v| walk_stmt(v, stmt));
            }
            // A condition or loop header declares into the enclosing scope;
            // the bodies are scopes of their own.
            // A statement modifier opens no scope: `my $x = 1 for ^1; $x`.
            Stmt::If {
                is_statement_modifier: true,
                ..
            }
            | Stmt::While {
                is_statement_modifier: true,
                ..
            }
            | Stmt::For {
                is_statement_modifier: true,
                ..
            }
            | Stmt::Given {
                is_statement_modifier: true,
                ..
            } => self.judging(false, Mode::Off, |v| walk_stmt(v, stmt)),
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
                ..
            } => self.judging(false, Mode::Off, |v| {
                v.visit_expr(cond);
                for branch in [then_branch, else_branch] {
                    v.in_scope(|v| {
                        if let Some(b) = binding_var {
                            v.declare(b);
                        }
                        walk_stmts(v, branch);
                    });
                }
            }),
            Stmt::While { cond, body, .. } => self.judging(false, Mode::Off, |v| {
                v.visit_expr(cond);
                v.in_scope(|v| walk_stmts(v, body));
            }),
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => self.judging(false, Mode::Off, |v| {
                if let Some(init) = init {
                    walk_stmts(v, std::slice::from_ref(init));
                }
                for e in [cond, step].into_iter().flatten() {
                    v.visit_expr(e);
                }
                v.in_scope(|v| walk_stmts(v, body));
            }),
            Stmt::SubDecl { param_defs, .. } | Stmt::MethodDecl { param_defs, .. } => {
                self.routine(param_defs, stmt)
            }
            Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } => {
                self.type_body(body, stmt)
            }
            // A block's statements are judged as the snippet's are.
            Stmt::Block(_) => {
                self.in_scope(|v| v.judging(judged, Mode::Off, |v| walk_stmt(v, stmt)))
            }
            Stmt::SyntheticBlock(_) => self.judging(judged, Mode::Off, |v| walk_stmt(v, stmt)),
            // Every other statement with a body opens a scope for it (its
            // parameters, reported as names, are declared there).
            Stmt::For { .. }
            | Stmt::Given { .. }
            | Stmt::When { .. }
            | Stmt::Default(_)
            | Stmt::Catch(_)
            | Stmt::Control(_)
            | Stmt::Phaser { .. }
            | Stmt::Whenever { .. }
            | Stmt::React { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::Package { .. }
            | Stmt::AugmentClass { .. } => {
                self.in_scope(|v| v.judging(false, Mode::Off, |v| walk_stmt(v, stmt)))
            }
            _ => self.judging(false, Mode::Off, |v| walk_stmt(v, stmt)),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found.is_some() {
            return;
        }
        let mode = self.mode;
        match expr {
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) => {
                if mode != Mode::Off {
                    self.check_var(expr);
                }
            }
            // An embedded declaration on a judged right-hand side keeps its
            // initializer judged (see the `VarDecl` arm of `visit_stmt`).
            Expr::DoStmt(inner)
                if mode == Mode::Ordered && matches!(**inner, Stmt::VarDecl { .. }) =>
            {
                walk_expr(self, expr)
            }
            // Closures and inline blocks are scopes of their own.
            Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Block(_)
            | Expr::Gather(_)
            | Expr::DoBlock { .. }
            | Expr::Try { .. }
            | Expr::PhaserExpr { .. }
            | Expr::Once { .. } => {
                self.in_scope(|v| v.judging(false, Mode::Off, |v| walk_expr(v, expr)))
            }
            Expr::StringInterpolation(_) | Expr::Index { .. } | Expr::MethodCall { .. } => {
                let inner = if mode == Mode::Off {
                    Mode::Off
                } else {
                    Mode::Plain
                };
                self.judging(false, inner, |v| walk_expr(v, expr));
            }
            Expr::Binary { .. }
            | Expr::MetaOp { .. }
            | Expr::Grouped(_)
            | Expr::Unary { .. }
            | Expr::PostfixOp { .. }
            | Expr::Itemize(_) => {
                let inner = if mode == Mode::Ordered {
                    Mode::Ordered
                } else {
                    Mode::Off
                };
                self.judging(false, inner, |v| walk_expr(v, expr));
            }
            _ => self.judging(false, Mode::Off, |v| walk_expr(v, expr)),
        }
    }

    // Regex-internal declarations (`:my $x`) and match variables are not
    // modelled, so a regex is not entered.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::Param | NameKind::BlockParam) {
            self.declare(name);
        }
    }
}

/// A parameter's `where` constraint is checked left-to-right: it may name
/// the parameter it constrains and any *earlier* parameter, but referencing a
/// *later* parameter is `X::Undeclared` (that param is not in scope yet).
/// Matches `sub foo($x where { $x == $y }, $y) {}` → X::Undeclared.
/// (subtypes.t 77)
fn later_param_in_where(param_defs: &[ParamDef]) -> Option<Undeclared> {
    let named: Vec<&ParamDef> = param_defs.iter().filter(|pd| !pd.name.is_empty()).collect();
    for (i, pd) in named.iter().enumerate() {
        let Some(where_expr) = &pd.where_constraint else {
            continue;
        };
        let mut refs = VarRefs::default();
        refs.visit_expr(where_expr);
        let earlier = |r: &str| named[..=i].iter().any(|p| bare_var_name(&p.name) == r);
        if let Some(r) = refs.names.iter().find(|r| {
            named[i + 1..]
                .iter()
                .any(|p| bare_var_name(&p.name) == r.as_str())
                && !earlier(r)
        }) {
            return Some(("$", r.clone(), Vec::new()));
        }
    }
    None
}

/// The names of the variables (`$`, `@`, `%`, `&`) an expression refers to.
#[derive(Default)]
struct VarRefs {
    names: Vec<String>,
}

impl<'ast> Visit<'ast> for VarRefs {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Var(n) | Expr::ArrayVar(n) | Expr::HashVar(n) | Expr::CodeVar(n) = expr {
            self.names.push(n.clone());
        }
        walk_expr(self, expr);
    }
}
