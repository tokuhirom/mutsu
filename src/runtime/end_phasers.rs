//! `END` phaser installation, capture and exit-time freezing.
//!
//! ## Installing the main compunit's ENDs before its body runs
//!
//! rakudo installs an `END` when the compunit that declares it is *compiled*,
//! walking the source top to bottom, and runs the installed phasers in reverse
//! at exit. Two consequences fall out of that which "register when execution
//! reaches the declaration" cannot reproduce:
//!
//! * An `END` inside a block that never runs, or a sub that is never called,
//!   still runs at exit:
//!
//!   ```raku
//!   if False { END { say "never-run-block" } }
//!   sub g    { END { say "uncalled-sub" } }
//!   END      { say "main" }
//!   # main, uncalled-sub, never-run-block
//!   ```
//!
//! * The run order is reverse *source* order across the whole compunit, with
//!   no tie between two `END`s that share one physical line.
//!
//! So this pass walks the parsed main compunit in source order and installs
//! every `END` it finds, at the source-order index the parser stamped on the
//! declaration ([`crate::ast::Stmt::Phaser::end_index`]). Execution reaching
//! one of those declarations later does not install a second phaser; it only
//! records the lexical scope the body closes over, into the slot this pass
//! already gave it (`Interpreter::capture_end_phaser_env`). A phaser execution
//! never reaches keeps the empty env installed here, and its body then sees
//! the declaring block's lexicals as undefined — which is what rakudo shows
//! for a block that never ran.
//!
//! A nesting form this walker fails to descend into degrades gracefully: that
//! `END` is simply not pre-installed, so it behaves as it did before this pass
//! existed (installed when reached). Its *ordering* is unaffected either way,
//! because the order key is the parser's index, not this walk.
//!
//! ## The rest of the file
//!
//! The `Interpreter` accessors that install a phaser, capture a scope into one,
//! and freeze one against a dying scope. They live here rather than with the
//! other misc accessors because the pre-pass above is what decides which of
//! them a given `END` goes through.

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::env::Env;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;

impl Interpreter {
    /// Install every `END` declared anywhere in the main compunit, in source
    /// order. See the module docs.
    pub(crate) fn preregister_main_end_phasers(&mut self, stmts: &[Stmt]) {
        let package = self.current_package();
        let mut walker = EndWalker {
            interp: self,
            package,
            lexicals: Vec::new(),
            depth: 0,
        };
        walker.stmts(stmts);
    }

    /// Register an END phaser found while *running*: inside a module body (in
    /// which case rakudo would have installed it at the `use`, before anything
    /// the main compunit declares) or inside an `EVAL` / an rvalue `END` of
    /// the main compunit. `end_index` is the source-order number the parser
    /// gave the declaration; pass `None` when the registration is not part of
    /// the main compunit's source numbering (a module body, an `EVAL`).
    ///
    /// The main compunit's ordinary ENDs do NOT come through here: they are
    /// all installed up front by `preregister_main_end_phasers`, and reaching
    /// one only re-captures its env (`capture_end_phaser_env`).
    pub(crate) fn push_end_phaser(&mut self, body: Vec<Stmt>, end_index: Option<u32>) {
        match self.module_load_order.last().copied() {
            // Inside a module body: install order is the load order, and the
            // module's own source numbering says nothing about the main
            // compunit.
            Some(base) => self.push_end_phaser_ordered(body, base, None),
            // Inside the main compunit but without a pre-installed slot: an
            // `EVAL`'d snippet (compiled at run time, so it installs after
            // everything the compunit declared) or an rvalue `END { }`.
            None => match end_index {
                Some(index) => {
                    self.push_end_phaser_ordered(body, super::end_order::MAIN, Some(index))
                }
                None => self.push_end_phaser_ordered(body, super::end_order::RUNTIME, None),
            },
        }
    }

    /// Install one of the main compunit's END phasers ahead of the body, at
    /// the source-order position `end_index`, and remember its slot so that
    /// reaching the declaration later re-captures into it.
    ///
    /// rakudo installs every END in a compunit when the compunit is compiled,
    /// so one inside a block that never runs — or a sub that is never called —
    /// still runs at exit. Such a phaser never captures a scope, and its body
    /// then sees the declaring block's lexicals the way rakudo does: as
    /// undefined, because nothing ever assigned them.
    pub(crate) fn preinstall_end_phaser(
        &mut self,
        body: Vec<Stmt>,
        end_index: u32,
        package: String,
        lexicals: &[String],
    ) {
        // Idempotent per index. The walker can reach one declaration twice
        // through an AST node that keeps both a source form and its expansion;
        // installing a second phaser for it would run the body twice.
        if self.main_end_slots.contains_key(&end_index) {
            return;
        }
        // No registration sequence is involved: the source-order index IS the
        // install position for a main-compunit END, which is the whole point
        // of numbering them at parse time.
        let order = super::end_order::MAIN + super::end_order::slot(Some(end_index), 0);
        let slot = self.end_phasers.len();
        // Seeded rather than cloned from the pre-run env: rakudo's END is a
        // closure that was never CLONED against a live frame, so every `my`/
        // `state` lexical it mentions reads as that container's *unassigned*
        // value -- `Any` for a `$`, an empty `Array`/`Hash` for `@`/`%` --
        // no matter which enclosing scope declared it and no matter what that
        // scope later stored there. Measured: `my $t = 5; if False { END {
        // say $t } }` prints `Any`, not 5, and `my @arr = 1,2,3; if False {
        // END { say @arr } }` prints `[]`. Only these names are seeded, so
        // everything that is NOT a per-frame lexical container -- routines,
        // `our`/package variables, constants, types, dynamics -- still
        // resolves against the live exit-time env, which is also what rakudo
        // does.
        let mut env = Env::new();
        let mut dead_keys = crate::runtime::NameSet::default();
        for name in lexicals {
            let sym = Symbol::intern(name);
            env.insert_sym(sym, Self::unassigned_lexical_value(name));
            // The seed is this phaser's authoritative binding for the name, so
            // it must win over a live same-named variable further out at exit
            // (`my $w = 1; if False { my $w = 2; END { say $w } }` is `Any`).
            dead_keys.insert(sym);
        }
        self.end_phasers.push(super::EndPhaser {
            body,
            env,
            package,
            dead_keys,
            order,
            capture_seq: None,
        });
        self.main_end_slots.insert(end_index, slot);
    }

    /// The value an unassigned lexical container of this sigil reads as: `Any`
    /// for a `$` (and for a routine parameter that was never bound), an empty
    /// `Array` for an `@`, an empty `Hash` for a `%`.
    ///
    /// rakudo answers `VMNull` for an uncalled routine's parameter — a raw NQP
    /// null that explodes on any method call (`.defined` throws
    /// `X::Method::NotFound ... for invocant of type 'VMNull'`). That is an
    /// implementation artifact of its binder, not a Raku value; mutsu
    /// deliberately answers `Any` there instead, which agrees with rakudo on
    /// every *defined* question (`.defined` is `False` either way).
    fn unassigned_lexical_value(name: &str) -> crate::value::Value {
        match name.chars().next() {
            Some('@') => crate::value::Value::real_array(Vec::new()),
            Some('%') => crate::value::Value::hash(std::collections::HashMap::new()),
            // Scalars are stored under their BARE name (no `$`), the same
            // convention `Stmt::VarDecl::name` uses — see `push_lexical`.
            _ => crate::value::Value::package(Symbol::intern("Any")),
        }
    }

    /// Capture the current lexical scope into the pre-installed phaser slot
    /// for `end_index`, if there is one. Returns false when there is not (a
    /// module's, an `EVAL`'s, or a synthesized END), leaving the caller to
    /// install a fresh phaser.
    ///
    /// Re-reaching a declaration re-captures, so the LAST execution wins —
    /// which is what rakudo does: `sub f($n) { my $v = $n; END { say $v } }`
    /// called with 1, 2, 3 prints 3, and a `for` loop's END sees the final
    /// iteration.
    pub(crate) fn capture_end_phaser_env(&mut self, end_index: Option<u32>) -> bool {
        let Some(slot) = end_index.and_then(|i| self.main_end_slots.get(&i).copied()) else {
            return false;
        };
        let captured_env = self.env.clone();
        let package = self.current_package();
        let mark = self.end_phaser_capture_seq;
        self.end_phaser_capture_seq += 1;
        let phaser = &mut self.end_phasers[slot];
        phaser.env = captured_env;
        phaser.package = package;
        // A fresh capture supersedes whatever the previous one froze.
        phaser.dead_keys = crate::runtime::NameSet::default();
        phaser.capture_seq = Some(mark);
        true
    }

    fn push_end_phaser_ordered(&mut self, body: Vec<Stmt>, order_base: u64, index: Option<u32>) {
        let captured_env = self.env.clone();
        let package = self.current_package();
        let order = order_base + super::end_order::slot(index, self.end_phaser_seq);
        self.end_phaser_seq += 1;
        let mark = self.end_phaser_capture_seq;
        self.end_phaser_capture_seq += 1;
        self.end_phasers.push(super::EndPhaser {
            body,
            env: captured_env,
            package,
            dead_keys: crate::runtime::NameSet::default(),
            order,
            capture_seq: Some(mark),
        });
    }

    /// A mark of "how many END-phaser env captures have happened so far",
    /// taken on scope entry and handed back to [`update_end_phaser_envs`] on
    /// scope exit to name the captures this scope is responsible for freezing.
    ///
    /// This replaced the old `end_phasers.len()` mark, which stopped meaning
    /// anything once every main-compunit END was installed up front: with
    /// pre-installation the vector no longer grows when a scope registers a
    /// phaser, it only re-captures.
    pub(crate) fn end_phaser_capture_mark(&self) -> u64 {
        self.end_phaser_capture_seq
    }

    /// True when at least one END phaser is registered.
    pub(crate) fn has_end_phasers(&self) -> bool {
        !self.end_phasers.is_empty()
    }

    /// Freeze the END phasers that captured at or after `since_mark` against a
    /// scope that is about to die: refresh their captured values from that
    /// scope's final env, and record `dying` as the keys the scope takes with
    /// it.
    ///
    /// A frozen key is the *only* surviving binding of that name for this
    /// phaser, so at exit it wins over any live same-named variable further out
    /// (`{ my $a = 42; END { say $a } }`). A key that is not frozen still names
    /// a live variable, and the live value — including mutations made after
    /// registration — is what the phaser must see.
    pub(crate) fn update_end_phaser_envs(
        &mut self,
        since_mark: u64,
        current_env: &Env,
        dying: &crate::runtime::NameSet,
    ) {
        for phaser in self
            .end_phasers
            .iter_mut()
            .filter(|p| p.capture_seq.is_some_and(|seq| seq >= since_mark))
        {
            let captured = &mut phaser.env;
            for (k, v) in current_env {
                if captured.contains_key_sym(*k) {
                    captured.insert_sym(*k, v.clone());
                }
            }
            for k in dying {
                if captured.contains_key_sym(*k) {
                    phaser.dead_keys.insert(*k);
                }
            }
        }
    }

    /// Update captured envs of ALL END phasers, but only for the specified
    /// variable names.  Used after closure calls to propagate changes to
    /// captured variables without overwriting unrelated variables.
    ///
    /// `keys` names the *calling closure's* own captured free variables, which
    /// only coincidentally share a name with a phaser's captured entry — they
    /// are not necessarily the same binding (a same-named `my` in a sibling
    /// scope is a common case: `{ my $a = 42; END { say $a } }; my $a = 0;
    /// callit { $a }` calls a closure that captured the SECOND `$a`, which must
    /// not clobber the phaser's captured FIRST `$a`). A key already in
    /// `phaser.dead_keys` (frozen at the moment its own declaring scope died —
    /// see `update_end_phaser_envs`) is the phaser's authoritative surviving
    /// binding for that name and must never be overwritten by an unrelated
    /// same-named capture from elsewhere.
    pub(crate) fn update_end_phaser_envs_for_keys(
        &mut self,
        keys: &std::collections::HashSet<&str>,
        current_env: &Env,
    ) {
        for phaser in self.end_phasers.iter_mut() {
            let captured = &mut phaser.env;
            for k in keys {
                if phaser.dead_keys.contains(&Symbol::intern(k)) {
                    continue;
                }
                if captured.contains_key(k)
                    && let Some(v) = current_env.get(k)
                {
                    captured.insert(k.to_string(), v.clone());
                }
            }
        }
    }

    /// Register an END phaser site_id. Returns true if this is the first
    /// registration (phaser should be pushed), false if already registered.
    pub(crate) fn register_end_phaser_site(&mut self, site_id: u64) -> bool {
        self.end_phaser_sites.insert(site_id)
    }
}

struct EndWalker<'a> {
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
}

impl EndWalker<'_> {
    fn stmts(&mut self, stmts: &[Stmt]) {
        let mark = self.lexicals.len();
        self.depth += 1;
        for s in stmts {
            // Walk first, then record: a `my $x` is visible to what FOLLOWS it,
            // which is the only place an `END` could legally mention it.
            self.stmt(s);
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

    /// Record the lexical a statement declares, if it declares one.
    fn declare(&mut self, stmt: &Stmt) {
        if let Stmt::VarDecl {
            name,
            is_our: false,
            is_dynamic: false,
            ..
        } = stmt
        {
            self.push_lexical(name);
        }
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
        if name.contains("::") {
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
        self.stmts(body);
        self.package = saved;
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Phaser {
                kind: PhaserKind::End,
                body,
                end_index,
                ..
            } => {
                if let Some(index) = *end_index {
                    // Only at `GLOBAL`: inside a `class`/`role`/`package` body
                    // the compiler package-qualifies a lexical's env key
                    // (`Compiler::qualify_variable_name`), and this walker does
                    // not reproduce that mangling — seeding a bare name there
                    // would install a binding the body never reads. Such an
                    // `END` keeps the pre-seeding behaviour (it resolves against
                    // the live exit-time env).
                    let lexicals: &[String] = if self.package == "GLOBAL" && self.depth > 1 {
                        &self.lexicals
                    } else {
                        &[]
                    };
                    self.interp.preinstall_end_phaser(
                        body.clone(),
                        index,
                        self.package.clone(),
                        lexicals,
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
            | Stmt::React { body }
            | Stmt::Loop { body, .. }
            | Stmt::Subtest { body, .. } => self.stmts(body),
            Stmt::SubDecl { params, body, .. }
            | Stmt::MethodDecl { params, body, .. }
            | Stmt::ProtoDecl { params, body, .. } => {
                let params: Vec<&str> = params.iter().map(String::as_str).collect();
                self.param_scope(&params, body);
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
                self.expr(cond);
                self.stmts(then_branch);
                self.stmts(else_branch);
            }
            Stmt::While { cond, body, .. } | Stmt::When { cond, body, .. } => {
                self.expr(cond);
                self.stmts(body);
            }
            Stmt::For {
                iterable,
                param,
                params,
                body,
                ..
            } => {
                self.expr(iterable);
                let mut names: Vec<&str> = params.iter().map(String::as_str).collect();
                if let Some(p) = param {
                    names.push(p.as_str());
                }
                self.param_scope(&names, body);
            }
            Stmt::Given { topic, body, .. } => {
                self.expr(topic);
                self.stmts(body);
            }
            Stmt::Whenever { supply, body, .. } => {
                self.expr(supply);
                self.stmts(body);
            }
            Stmt::Label { stmt, .. } => self.stmt(stmt),
            Stmt::Expr(e)
            | Stmt::Return(e)
            | Stmt::Die(e)
            | Stmt::Fail(e)
            | Stmt::Goto(e)
            | Stmt::Take(e, _) => self.expr(e),
            Stmt::VarDecl { expr: e, .. } | Stmt::Assign { expr: e, .. } => self.expr(e),
            Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
                for e in es {
                    self.expr(e);
                }
            }
            Stmt::Call { args, .. } => {
                for a in args {
                    self.call_arg(a);
                }
            }
            Stmt::Let { value, index, .. } => {
                if let Some(e) = value {
                    self.expr(e);
                }
                if let Some(e) = index {
                    self.expr(e);
                }
            }
            _ => {}
        }
    }

    fn call_arg(&mut self, arg: &crate::ast::CallArg) {
        match arg {
            crate::ast::CallArg::Positional(e)
            | crate::ast::CallArg::Slip(e)
            | crate::ast::CallArg::Invocant(e) => self.expr(e),
            crate::ast::CallArg::Named { value: Some(e), .. } => self.expr(e),
            crate::ast::CallArg::Named { value: None, .. } => {}
        }
    }

    fn expr(&mut self, expr: &Expr) {
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
            Expr::DoStmt(inner) => self.stmt(inner),
            Expr::WhateverCurry(inner)
            | Expr::Eager(inner)
            | Expr::Itemize(inner)
            | Expr::ZenSlice(inner)
            | Expr::Grouped(inner)
            | Expr::PositionalPair(inner)
            | Expr::DeitemizeForBind(inner)
            | Expr::AssignExpr { expr: inner, .. } => self.expr(inner),
            Expr::Index { target, index, .. } => {
                self.expr(target);
                self.expr(index);
            }
            Expr::IndexAssign {
                target,
                index,
                value,
                ..
            } => {
                self.expr(target);
                self.expr(index);
                self.expr(value);
            }
            // Only the expansion is executed; the preserved source halves are a
            // RakuAST-facing marker and hold the same nodes.
            Expr::CompoundAssign { expanded, .. } => self.expr(expanded),
            Expr::Feed { source, sink, .. } => {
                self.expr(source);
                self.expr(sink);
            }
            Expr::Binary { left, right, .. }
            | Expr::HyperOp { left, right, .. }
            | Expr::MetaOp { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            Expr::ChainedCompare { operands, .. } => {
                for o in operands {
                    self.expr(o);
                }
            }
            Expr::Unary { expr: inner, .. } | Expr::PostfixOp { expr: inner, .. } => {
                self.expr(inner)
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                self.expr(target);
                for a in args {
                    self.expr(a);
                }
            }
            Expr::CallOn { target, args } => {
                self.expr(target);
                for a in args {
                    self.expr(a);
                }
            }
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                for a in args {
                    self.expr(a);
                }
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.expr(cond);
                self.expr(then_expr);
                self.expr(else_expr);
            }
            Expr::ArrayLiteral(items)
            | Expr::BracketArray(items, _)
            | Expr::CaptureLiteral(items) => {
                for e in items {
                    self.expr(e);
                }
            }
            Expr::Hash(pairs) => {
                for e in pairs.iter().filter_map(|(_, v)| v.as_ref()) {
                    self.expr(e);
                }
            }
            _ => {}
        }
    }
}
