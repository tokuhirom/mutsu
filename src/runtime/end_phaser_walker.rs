//! Visit a unit's END declarations and their lexical scopes.

use super::*;
use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};

pub(super) struct EndWalker<'a> {
    interp: &'a mut Interpreter,
    /// Package the statements currently being walked belong to, so an `END`
    /// that is never reached still runs under the package it was declared in
    /// (a reached one has its package refreshed at capture time).
    package: String,
    /// Every `my`/`state` lexical (and routine/block parameter) declared
    /// *before* the current descent point, across all enclosing scopes — i.e.
    /// exactly the lexical containers an `END` declared here would close over.
    /// A never-reached phaser is seeded with these, since rakudo's uncloned
    /// closure reads them all as unassigned. Pushed on scope entry and
    /// truncated on exit, so it always describes the current point.
    lexicals: Vec<String>,
    /// Statement-list nesting depth: 1 while walking the unit's own top-level
    /// statements, higher inside any nested body. An `END` at depth 1 is
    /// ALWAYS reached (and `Interpreter::run` drops it from the body outright,
    /// since it closes over the still-live unit scope), so it must not be
    /// seeded — a seed would be the only binding it ever gets.
    depth: u32,
    /// The NESTED package path (`["D1", "D2"]`), as distinct from `package`,
    /// which is the innermost name an `END` runs under. Only
    /// [`EndWalker::install_our_symbol`] reads it -- see
    /// [`EndWalker::in_package`].
    package_path: Vec<String>,
    /// Whether an `END` found is installed (the main compunit's walk); an
    /// `EVAL` unit's walk installs only its `our` symbols, its ENDs register
    /// when they are reached.
    end_order: Option<u64>,
}

impl EndWalker<'_> {
    pub(super) fn new(interp: &mut Interpreter, end_order: Option<u64>) -> EndWalker<'_> {
        EndWalker {
            package: interp.current_package(),
            interp,
            lexicals: Vec::new(),
            depth: 0,
            package_path: Vec::new(),
            end_order,
        }
    }

    pub(super) fn stmts(&mut self, stmts: &[Stmt]) {
        let mark = self.lexicals.len();
        self.depth += 1;
        for s in stmts {
            // Walk first, then record: a `my $x` is visible to what FOLLOWS it,
            // which is the only place an `END` could legally mention it.
            self.visit_stmt(s);
            self.declare(s);
        }
        self.depth -= 1;
        self.lexicals.truncate(mark);
    }

    /// Walk `body` with `params` in scope — a routine or pointy-block body,
    /// whose parameters are lexicals of that body just like its `my`s.
    fn param_scope(&mut self, params: &[&str], body: &[Stmt]) {
        let mark = self.lexicals.len();
        for p in params {
            self.push_lexical(p);
        }
        self.stmts(body);
        self.lexicals.truncate(mark);
    }

    /// Record the lexical a statement declares, if it declares one — and
    /// install the package SYMBOL of an `our` declaration.
    fn declare(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl {
                name,
                is_our: false,
                is_dynamic: false,
                constant,
                ..
            } => self.push_lexical(&crate::runtime::term_names::decl_storage_name(
                name, *constant,
            )),
            Stmt::VarDecl {
                name,
                is_our: true,
                type_constraint,
                constant,
                ..
            } => {
                // A sigil-less constant's lexical alias and unqualified
                // package symbol live under its term key (#9962), exactly
                // where the compiler stores them.
                let storage = crate::runtime::term_names::decl_storage_name(name, *constant);
                let name = &storage;
                self.install_our_symbol(name, type_constraint.as_deref());
                // An `our` inside a nested block also creates a LEXICAL alias
                // there, so a never-reached `END` in that block reads it as
                // unassigned exactly like a `my` would. Seeding it through the
                // same scoped `lexicals` stack is what keeps the alias
                // block-scoped: putting the bare name in the env instead would
                // make `{ our $sa2 = 42 }; $sa2` resolve, where raku reports
                // `X::Undeclared`.
                //
                // A UNIT-TOP-LEVEL `our` (depth 1) is skipped: it always runs,
                // so its package symbol holds a real value at exit and a
                // never-reached `END` must read THAT, not a seed
                // (`our $pkg = 7; if False { END { say $pkg } }` is `7`).
                if self.depth > 1 {
                    self.push_lexical(name);
                }
            }
            _ => {}
        }
    }

    /// Install an `our` declaration's package symbol, bound to its type object.
    ///
    /// rakudo installs a package symbol when the compunit is COMPILED, so the
    /// slot exists (undefined) even when the declaration sits in a branch that
    /// never runs: `if False { our $o = 4 }; say OUR::<$o>.^name` is `Any`.
    /// mutsu installs the symbol when execution REACHES the declaration, so a
    /// dead branch installed nothing and a later read found no binding (`Nil`).
    ///
    /// Only the symbol is installed, never a value: an assignment that DOES run
    /// overwrites the type object through the ordinary declaration path, and a
    /// name that already has a binding is left alone, so this cannot disturb a
    /// reached declaration or a symbol some earlier compunit set.
    fn install_our_symbol(&mut self, name: &str, type_constraint: Option<&str>) {
        // Same key the compiler's `qualify_our_variable_name` produces: the
        // walker's `package` is the ENCLOSING package (it descends into
        // `class`/`role`/`module` bodies but not into sub bodies), which is
        // exactly what that function qualifies an `our` declaration against.
        let Some(key) = Self::our_symbol_key(&self.our_package(), name) else {
            return;
        };
        if self.interp.has_our_var(&key) {
            return;
        }
        // An `@`/`%` `our` is a container, and rakudo gives it an EMPTY one
        // rather than a type object (`if False { our @a }; OUR::<@a>` is `[]`).
        let value = match name.chars().next() {
            Some('@') => crate::value::Value::real_array(Vec::new()),
            Some('%') => crate::value::Value::hash(ValueMap::default()),
            // `&` names are routines, which are installed by their own
            // declaration walk; leave them alone.
            Some('&') => return,
            _ => crate::value::Value::package(Symbol::intern(type_constraint.unwrap_or("Any"))),
        };
        // Never the BARE name in the env: the env is the lexical store here,
        // and an `our`'s lexical alias belongs to its declaring block, so
        // writing the bare name there would leak it out (`{ our $sa2 = 42 };
        // $sa2` must still be `X::Undeclared`). The block-scoped half of the
        // declaration is the `push_lexical` in `declare`.
        //
        // A package-qualified key (`$Bar::c`) is not a bare name and cannot
        // leak, and the env IS where `package_stash_value` looks for a named
        // package's symbols -- so that is where it goes. A GLOBAL-scope `our`
        // has no qualified spelling, and its stash (`OUR::`, `GLOBAL::.<>`)
        // reads the flat `our_vars` store instead.
        // The `our_vars` package store, never the env. The env is also the
        // LEXICAL store, and an `our`'s lexical alias belongs to its declaring
        // block: writing a bare name there leaks it out of the block
        // (`{ our $sa2 = 42 }; $sa2` must stay `X::Undeclared`), and writing a
        // qualified one perturbs the declaration path that later runs for a
        // reached declaration. `our_vars` is exactly the package-symbol store
        // the stash reads, and every consumer that could mistake this entry for
        // a live variable (`our_package_scalar_cell`) requires it to be a
        // `ContainerRef`, which a type object is not.
        self.interp.set_our_var(key, value);
    }

    /// The env key an `our NAME` declared in `package` is stored under, or
    /// `None` for a name that is not a package variable at all (a twigil, a
    /// positional capture, an already-qualified name).
    fn our_symbol_key(package: &str, name: &str) -> Option<String> {
        if name.is_empty() {
            return None;
        }
        let name_sym = Symbol::intern(name);
        if crate::qualified::is_qualified(name_sym) {
            return None;
        }
        let first = name.chars().next().unwrap();
        if matches!(first, '_' | '/' | '!' | '?' | '*' | '.' | '=')
            || (first.is_ascii_digit() && name.chars().all(|c| c.is_ascii_digit()))
        {
            return None;
        }
        if matches!(first, '$' | '@' | '%' | '&') && name.len() > 1 {
            let twigil = name[1..].chars().next();
            if matches!(twigil, Some('_' | '/' | '!' | '?' | '*' | '.' | '=')) {
                return None;
            }
        }
        let package_sym = Symbol::intern(package);
        if package_sym == crate::symbol::wk::global_package()
            || crate::qualified::is_routine_scoped_package(package_sym)
        {
            return Some(name.to_string());
        }
        // A term key is the unqualified spelling only; the package store of
        // a constant is keyed by its spelling (#9962).
        if let Some(term) = crate::runtime::term_names::term_spelling(name) {
            return Some(
                crate::qualified::qualified(Symbol::intern(package), Symbol::intern(term))
                    .as_str()
                    .to_string(),
            );
        }
        // The sigil stays in front of the package: `$x` -> `$P::x`.
        let key = match first {
            '$' | '@' | '%' | '&' if name.len() > 1 => {
                crate::qualified::qualified_var(package_sym, name_sym)
            }
            _ => crate::qualified::qualified(package_sym, name_sym),
        };
        Some(key.as_str().to_string())
    }

    /// Add one name to the visible set, if it is a per-frame lexical container.
    ///
    /// Names arrive in the AST's own spelling: an `@`/`%` variable keeps its
    /// sigil, a `$` variable is stored BARE (`my $x` is `VarDecl { name: "x" }`,
    /// and the topic is `"_"`), which is also how the env keys them.
    ///
    /// `&`-sigiled names, sigilless bindings and every twigil (`$*dyn`,
    /// `$!attr`, `$?FILE`, `$/`, `$_`) are skipped: they are not frame lexicals
    /// whose value an uncloned closure would lose, and rakudo resolves them
    /// normally inside a never-reached `END` (measured — an uncalled `sub`, an
    /// `our` variable, a `constant`, a class and `$*PROGRAM-NAME` all still
    /// answer there).
    fn push_lexical(&mut self, name: &str) {
        if crate::qualified::is_qualified(Symbol::intern(name)) {
            return;
        }
        let bare = match name.chars().next() {
            Some('@' | '%') => &name[1..],
            Some('&' | '$') => return,
            _ => name,
        };
        // A twigil or a special name (`_`, `/`, `!`, `*x`, `?x`, `.x`, `0`) is
        // not an ordinary frame lexical.
        let mut chars = bare.chars();
        match chars.next() {
            Some(c) if c.is_alphabetic() || c == '_' => {}
            _ => return,
        }
        if bare == "_" || !chars.all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
            return;
        }
        if !self.lexicals.iter().any(|n| n == name) {
            self.lexicals.push(name.to_string());
        }
    }

    fn in_package(&mut self, name: &str, body: &[Stmt]) {
        let saved = std::mem::replace(&mut self.package, name.to_string());
        // The NESTED path is tracked separately from `package`: an `END`'s
        // package is the innermost name (what it runs under), while an `our`
        // declaration's storage key is qualified against the full path the
        // compiler's `current_package` carries -- `package D1 { package D2 {
        // our $d3 } }` stores `D1::D2::d3`, and keying the pre-install on the
        // bare `D2` instead shadowed the real symbol for `$D2::d3`.
        self.package_path.push(name.to_string());
        self.stmts(body);
        self.package_path.pop();
        self.package = saved;
    }

    /// The package an `our` declared at this point qualifies against: the
    /// nested path, empty at the unit's top level (where the compiler collapses
    /// to `GLOBAL`).
    fn our_package(&self) -> String {
        if self.package_path.is_empty() {
            self.package.clone()
        } else {
            self.package_path.join("::")
        }
    }
}

/// The walk itself (ADR-0137 visitor). The constructs that open a lexical
/// scope go through [`EndWalker::stmts`] / [`EndWalker::param_scope`], which
/// track the declarations a nested `END` closes over; every other node takes
/// the visitor's default recursion, so an `END` anywhere in the compunit is
/// found. One reached only through that default recursion (a nested body the
/// arms below do not list) is still pre-installed, just without the seed of
/// that body's own lexicals -- it then resolves them against the live
/// exit-time env, as every `END` did before this pass existed.
impl<'ast> Visit<'ast> for EndWalker<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Phaser {
                kind: PhaserKind::End,
                body,
                end_index,
                ..
            } => {
                if let Some((index, order_base)) = (*end_index).zip(self.end_order) {
                    // Only at `GLOBAL`: inside a `class`/`role`/`package` body
                    // the compiler package-qualifies a lexical's env key
                    // (`Compiler::qualify_variable_name`), and this walker does
                    // not reproduce that mangling — seeding a bare name there
                    // would install a binding the body never reads. Such an
                    // `END` keeps the pre-seeding behaviour (it resolves against
                    // the live exit-time env).
                    let lexicals: &[String] = if Symbol::intern(&self.package)
                        == crate::symbol::wk::global_package()
                        && self.depth > 1
                    {
                        &self.lexicals
                    } else {
                        &[]
                    };
                    self.interp.preinstall_end_phaser(
                        body.clone(),
                        index,
                        self.package.clone(),
                        lexicals,
                        order_base,
                    );
                }
                // An END nested inside another END's body is installed by the
                // same rule, so keep descending.
                self.stmts(body);
            }
            Stmt::Phaser { body, .. } => self.stmts(body),
            Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::React { body, .. } => self.stmts(body),
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => {
                // `loop (my $i = 0; ...)` declares `$i` in the enclosing scope.
                if let Some(init) = init {
                    walk_stmts(self, std::slice::from_ref(init.as_ref()));
                    self.declare(init);
                }
                for e in [cond, step].into_iter().flatten() {
                    self.visit_expr(e);
                }
                self.stmts(body);
            }
            Stmt::SubDecl {
                params,
                param_defs,
                body,
                ..
            }
            | Stmt::MethodDecl {
                params,
                param_defs,
                body,
                ..
            }
            | Stmt::ProtoDecl {
                params,
                param_defs,
                body,
                ..
            } => {
                // A parameter default is code of the routine too.
                for p in param_defs {
                    self.visit_param(p);
                }
                let params: Vec<&str> = params.iter().map(String::as_str).collect();
                self.param_scope(&params, body);
            }
            // A lexical package is stored under its declaration's mangled
            // name (`X\0<id>`), so its `our` symbols are keyed under that
            // (#17449): the facing spelling `X::v` is another declaration's
            // as much as this one's.
            Stmt::Package {
                name,
                body,
                is_my: true,
                decl_id,
                ..
            }
            | Stmt::ClassDecl {
                name,
                body,
                is_lexical: true,
                decl_id,
                ..
            }
            | Stmt::RoleDecl {
                name,
                body,
                is_lexical: true,
                decl_id,
                ..
            } if *decl_id != 0 && !crate::qualified::is_qualified(*name) => {
                self.in_package(&format!("{}\u{0}{decl_id}", name.resolve()), body)
            }
            Stmt::Package { name, body, .. }
            | Stmt::ClassDecl { name, body, .. }
            | Stmt::RoleDecl { name, body, .. }
            | Stmt::AugmentClass { name, body, .. } => self.in_package(&name.resolve(), body),
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expr(cond);
                self.stmts(then_branch);
                self.stmts(else_branch);
            }
            Stmt::While { cond, body, .. } | Stmt::When { cond, body, .. } => {
                self.visit_expr(cond);
                self.stmts(body);
            }
            Stmt::For {
                iterable,
                param,
                params,
                body,
                ..
            } => {
                self.visit_expr(iterable);
                let mut names: Vec<&str> = params.iter().map(String::as_str).collect();
                if let Some(p) = param {
                    names.push(p.as_str());
                }
                self.param_scope(&names, body);
            }
            Stmt::Given { topic, body, .. } => {
                self.visit_expr(topic);
                self.stmts(body);
            }
            Stmt::Whenever { supply, body, .. } => {
                self.visit_expr(supply);
                self.stmts(body);
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Block(body)
            | Expr::AnonSub { body, .. }
            | Expr::Gather(body)
            | Expr::DoBlock { body, .. }
            | Expr::Once { body }
            | Expr::PhaserExpr { body, .. } => self.stmts(body),
            Expr::AnonSubParams { params, body, .. } => {
                let params: Vec<&str> = params.iter().map(String::as_str).collect();
                self.param_scope(&params, body);
            }
            Expr::Lambda { param, body, .. } => self.param_scope(&[param.as_str()], body),
            Expr::Try { body, catch } => {
                self.stmts(body);
                if let Some(c) = catch {
                    self.stmts(c);
                }
            }
            // Only the expansion is executed; the preserved source halves are a
            // RakuAST-facing marker and hold the same nodes. The expansion
            // itself is an assignment, never one of the scope arms above.
            Expr::CompoundAssign { expanded, .. } => walk_expr(self, expanded),
            _ => walk_expr(self, expr),
        }
    }
}
