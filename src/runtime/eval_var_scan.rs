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
//! **Uses** are judged in every position, as rakudo does. A free variable
//! the snippet does not declare may be a lexical of the *caller's* static
//! scopes, which rakudo's pads hold from their entry even when it is declared
//! later in the source (`{ sub e($s) { EVAL $s }; e('!$y.defined'); my $y }`
//! is fine). The run-time env does not hold such a name yet, so the compiler
//! records, for every chunk that calls `EVAL`, the names its enclosing static
//! scopes declare (`CompiledCode::eval_scope_names`, #10511), and the scan
//! accepts those too. A placeholder's bare name (`$b` next to `$^b`) is left
//! to the placeholder checks, which report rakudo's own message.

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
    // A placeholder (`$^b`, `$:b`) declares its bare name in its block; a
    // bare use of that name before the placeholder is the placeholder check's
    // own error (`check_placeholder_*`, with rakudo's "Perhaps you forgot a
    // 'sub'" message), not this scan's.
    let mut refs = VarRefs::default();
    walk_stmts(&mut refs, stmts);
    let placeholder_names = refs
        .names
        .iter()
        .filter_map(|n| n.strip_prefix(['^', ':']).map(str::to_string))
        .collect();
    let mut scan = UndeclaredVars {
        interp,
        placeholder_names,
        package_exports: super::eval_package_exports::package_exported_vars(stmts),
        scopes: vec![HashSet::new()],
        strict: true,
        judged_stmt: true,
        forward: None,
        found: None,
    };
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// The first variable the unit uses before its top level declares it
/// (#12800): `sub f { $zz = 42 }; my $zz = 1;` is `X::Undeclared` at
/// compile time in rakudo, as is `say $x; my $x`. A narrow form of
/// [`first_undeclared_var`] over a whole compunit: a use is reported only
/// when nothing in scope there declares it and the unit's top level declares
/// that name later. Any other undeclared use is left to run time, as are
/// names a `use` of a module whose exported variables the parse-time scan
/// does not know may provide.
// Cost: O(n * d), n = size of the unit's AST, d = depth of the scope stack.
pub(super) fn first_var_used_before_unit_declares(
    interp: &Interpreter,
    stmts: &[Stmt],
) -> Option<Undeclared> {
    let mut top_names = HashSet::new();
    collect_top_level_decls(stmts, &mut top_names);
    if top_names.is_empty() {
        return None;
    }
    let mut refs = VarRefs::default();
    walk_stmts(&mut refs, stmts);
    let placeholder_names = refs
        .names
        .iter()
        .filter_map(|n| n.strip_prefix(['^', ':']).map(str::to_string))
        .collect();
    let mut scan = UndeclaredVars {
        interp,
        placeholder_names,
        package_exports: super::eval_package_exports::package_exported_vars(stmts),
        scopes: vec![HashSet::new()],
        strict: true,
        judged_stmt: true,
        forward: Some(ForwardUses {
            top_names,
            bail: false,
        }),
        found: None,
    };
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// The variable names the unit's top level declares, in `declared_var_name`
/// form, including those of a desugared multi-declaration.
// Cost: O(t), t = top-level statements.
fn collect_top_level_decls(stmts: &[Stmt], out: &mut HashSet<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl { name, .. } => {
                out.insert(declared_var_name(name).to_string());
            }
            Stmt::SyntheticBlock(inner) => collect_top_level_decls(inner, out),
            _ => {}
        }
    }
}

/// [`first_var_used_before_unit_declares`]'s mode of the scan.
struct ForwardUses {
    /// What the unit's top level declares.
    top_names: HashSet<String>,
    /// A `use` whose exported variables are unknown was seen: nothing is
    /// reported from here on.
    bail: bool,
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
        || crate::qualified::is_qualified_str(name)
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
    /// Bare names of the snippet's placeholder variables (see
    /// `first_undeclared_var`).
    placeholder_names: HashSet<String>,
    /// The variables each package the snippet declares exports, which an
    /// `import` of it declares.
    package_exports: std::collections::HashMap<String, Vec<String>>,
    /// Lexical scopes, innermost last, of names in `declared_var_name` form.
    scopes: Vec<HashSet<String>>,
    /// `no strict` is in effect: an assignment declares its target and no
    /// use is judged.
    strict: bool,
    /// The narrow compunit mode ([`first_var_used_before_unit_declares`]).
    forward: Option<ForwardUses>,
    /// The statement being visited is a statement of a statement list (not
    /// one embedded in an expression): its assignment target is judged.
    judged_stmt: bool,
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

    /// Whether `name` is a file-scope lexical of the running routine's own
    /// compunit, which is not in `env` (an imported `our $x is export` among
    /// them): the two unit-lexical redirects of the by-name read chokepoint
    /// (`get_env_with_main_alias`), without its package-variable fallbacks,
    /// which would make an `our` of an unrelated block look declared.
    // Cost: O(1) hash probes.
    fn unit_lexical_declares(&self, name: &str) -> bool {
        self.interp.unit_scope_lexical(name, None).is_some()
            || (self.interp.any_lexical_package_frame()
                && self.interp.module_imported_lexical(name).is_some())
    }

    /// Whether a static scope around the `EVAL` call declares `name`, here or
    /// later in its source (`CompiledCode::eval_scope_names`): the run-time
    /// env does not hold a lexical declared after the call yet (#10511).
    // Cost: O(|name|).
    fn caller_scope_declares(&self, name: &str) -> bool {
        if self.interp.current_code == 0 {
            return false;
        }
        // SAFETY: `current_code` is the address of the live bytecode frame's
        // `CompiledCode` (the frame calling `EVAL`), kept alive for the whole
        // frame by `vm_call_*`.
        let code = unsafe { &*(self.interp.current_code as *const crate::opcode::CompiledCode) };
        code.eval_scope_names.contains(name)
    }

    /// Runs `f` with the given judging state, restoring the current one.
    fn judging(&mut self, judged_stmt: bool, f: impl FnOnce(&mut Self)) {
        let saved = self.judged_stmt;
        self.judged_stmt = judged_stmt;
        f(self);
        self.judged_stmt = saved;
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
        let found = match expr {
            Expr::Var(name) => self.undeclared_scalar(name),
            Expr::ArrayVar(name) => self.undeclared_container("@", name),
            Expr::HashVar(name) => self.undeclared_container("%", name),
            _ => None,
        };
        if let Some(forward) = &self.forward
            && let Some((sigil, name, _)) = &found
        {
            let key = if *sigil == "$" {
                name.clone()
            } else {
                format!("{sigil}{name}")
            };
            if forward.bail || !forward.top_names.contains(&key) {
                return;
            }
        }
        self.found = found;
    }

    fn undeclared_scalar(&self, name: &str) -> Option<Undeclared> {
        if is_exempt(name)
            || self.is_declared(name)
            || self.placeholder_names.contains(name)
            || self.caller_scope_declares(name)
        {
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
        // A file-scope lexical of the running routine's own compunit (an
        // imported `our $x is export` among them) is not in `env`: the by-name
        // read chokepoint redirects to the unit's store, and so does this.
        if (env.contains_key(name) || self.unit_lexical_declares(name))
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
        if is_exempt(name)
            || self.is_declared(name)
            || self.is_declared(&sigiled)
            || self.placeholder_names.contains(name)
            || self.caller_scope_declares(&sigiled)
        {
            return None;
        }
        let env = self.interp.env();
        if env.contains_key(&sigiled)
            || env.contains_key(name)
            || self.unit_lexical_declares(&sigiled)
        {
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
        self.in_scope(|v| v.judging(true, |v| walk_stmt(v, stmt)));
    }

    /// In the compunit mode, a `use` declares the variables its module
    /// exports, when the parse-time scan knows them; otherwise nothing after
    /// it is reported.
    fn forward_use(&mut self, module: &str, tags: &[String]) {
        let Some(forward) = &self.forward else {
            return;
        };
        if forward.bail {
            return;
        }
        match crate::parser::scanned_exported_vars(module) {
            Some(vars) => {
                for var in vars.iter().filter(|var| var.imported_by(tags)) {
                    self.declare(&var.name);
                }
            }
            None => {
                if let Some(forward) = &mut self.forward {
                    forward.bail = true;
                }
            }
        }
    }

    /// A class or role body's scope: its attributes (`has $.x` makes `$!x`
    /// available, the alias form `has $x` also `$x`) and its body lexicals,
    /// which every method of the body sees, wherever they are declared.
    fn type_body(&mut self, body: &[Stmt], stmt: &Stmt) {
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
            v.judging(true, |v| walk_stmt(v, stmt));
        });
    }
}

impl<'ast> Visit<'ast> for UndeclaredVars<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        match stmt {
            Stmt::No { module, .. } if module == "strict" => self.strict = false,
            Stmt::Use { module, .. } if module == "strict" => self.strict = true,
            Stmt::Use { module, tags, .. }
                if !crate::runtime::begin_prologue::is_known_pragma_name(module)
                    && module != "lib"
                    && !module.starts_with("MONKEY") =>
            {
                self.forward_use(module, tags)
            }
            Stmt::Import { module, .. } => {
                if let Some(names) = self.package_exports.get(module).cloned() {
                    for name in &names {
                        self.declare(name);
                    }
                }
            }
            // Only uses after it see the new name (`$foo ~ my $foo` is
            // undeclared); its own initializer already does (`my $x = $x`).
            Stmt::VarDecl { name, .. } => {
                self.declare(name);
                self.judging(false, |v| walk_stmt(v, stmt));
            }
            Stmt::Assign {
                name,
                expr,
                target_is_sigilless,
                ..
            } => {
                self.assign_target(name, *target_is_sigilless);
                self.judging(false, |v| v.visit_expr(expr));
            }
            Stmt::Expr(_) | Stmt::Say(_) | Stmt::Print(_) | Stmt::Put(_) | Stmt::Note(_) => {
                self.judging(false, |v| walk_stmt(v, stmt));
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
            } => self.judging(true, |v| walk_stmt(v, stmt)),
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
                ..
            } => self.judging(true, |v| {
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
            Stmt::While { cond, body, .. } => self.judging(true, |v| {
                v.visit_expr(cond);
                v.in_scope(|v| walk_stmts(v, body));
            }),
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => self.judging(true, |v| {
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
            // A statement-form phaser (`BEGIN my $c = ...`) declares into the
            // enclosing scope; its block form (`BEGIN { my $c }`, whose body
            // starts with a `SetLine`) is a scope of its own, below.
            Stmt::Phaser { body, .. }
                if matches!(body.as_slice(), [Stmt::VarDecl { .. }]) =>
            {
                self.judging(true, |v| walk_stmts(v, body))
            }
            // A block's statements are judged as the snippet's are.
            Stmt::Block(_) => {
                self.in_scope(|v| v.judging(true, |v| walk_stmt(v, stmt)))
            }
            Stmt::SyntheticBlock(_) => self.judging(true, |v| walk_stmt(v, stmt)),
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
                self.in_scope(|v| v.judging(true, |v| walk_stmt(v, stmt)))
            }
            _ => self.judging(true, |v| walk_stmt(v, stmt)),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found.is_some() {
            return;
        }
        match expr {
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) => self.check_var(expr),
            // A desugaring's block is no scope: `(my $a, my $b) »=» (1, 2)`
            // declares `$a` and `$b` where it is written.
            Expr::DoBlock {
                origin: crate::ast::DoBlockOrigin::Desugar,
                ..
            } => self.judging(true, |v| walk_expr(v, expr)),
            // Closures and inline blocks are scopes of their own.
            Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Block(_)
            | Expr::Gather(_)
            | Expr::DoBlock { .. }
            | Expr::Try { .. }
            | Expr::PhaserExpr { .. }
            | Expr::Once { .. } => self.in_scope(|v| v.judging(true, |v| walk_expr(v, expr))),
            // An assignment embedded in an expression declares nothing and
            // is judged as the expression it is part of.
            _ => self.judging(false, |v| walk_expr(v, expr)),
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
