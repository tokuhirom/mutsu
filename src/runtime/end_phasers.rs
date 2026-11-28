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

use crate::ast::Stmt;
use crate::env::Env;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::ValueMap;

#[path = "end_phaser_walker.rs"]
mod walker;
use walker::EndWalker;

impl Interpreter {
    /// Remove the `my $*x` bindings still held by the exit-time env (the
    /// mainline's frame is gone when `END` runs) and answer their env keys, so
    /// a captured END env does not bring them back.
    // Cost: O(e), e = env keys.
    pub(crate) fn drop_mainline_lexical_dynamics(&mut self) -> rustc_hash::FxHashSet<Symbol> {
        let prefix = crate::meta_ns::MetaNs::LexicalDynamic.prefix();
        let names: Vec<String> = self
            .env
            .keys()
            .filter_map(|k| k.with_str(|s| s.strip_prefix(prefix).map(str::to_string)))
            .collect();
        let mut dead = rustc_hash::FxHashSet::default();
        for name in names {
            self.env
                .remove(&crate::meta_ns::MetaNs::LexicalDynamic.owned_key_for_str(&name));
            for key in [name.clone(), format!("${name}")] {
                self.env.remove(&key);
                dead.insert(Symbol::intern(&key));
            }
        }
        dead
    }

    /// Install every `END` declared anywhere in the main compunit, in source
    /// order. See the module docs.
    pub(crate) fn preregister_main_end_phasers(&mut self, stmts: &[Stmt]) {
        let mut walker = EndWalker::new(self, Some(super::end_order::MAIN));
        walker.stmts(stmts);
    }

    /// Install an EVAL unit's ENDs before its body can run a nested EVAL.
    pub(crate) fn preregister_eval_end_phasers(&mut self, stmts: &[Stmt]) {
        let mut walker = EndWalker::new(self, Some(super::end_order::RUNTIME));
        walker.stmts(stmts);
    }

    /// Install the package symbol of every `our` declared anywhere in an
    /// `EVAL` unit, reached or not: rakudo installs it when the unit is
    /// compiled (`EVAL 'if False { our @a }; OUR::<@a>.elems'` is 0).
    // Cost: O(n), n = nodes of the unit.
    pub(crate) fn preinstall_unit_our_symbols(&mut self, stmts: &[Stmt]) {
        let mut walker = EndWalker::new(self, None);
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
        match self.control.module_load_order.last().copied() {
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
        order_base: u64,
    ) {
        // Idempotent per index. The walker can reach one declaration twice
        // through an AST node that keeps both a source form and its expansion;
        // installing a second phaser for it would run the body twice.
        if self.control.main_end_slots.contains_key(&end_index) {
            return;
        }
        // No registration sequence is involved: the source-order index IS the
        // install position for a main-compunit END, which is the whole point
        // of numbering them at parse time.
        let order = if order_base == super::end_order::MAIN {
            order_base + super::end_order::slot(Some(end_index), 0)
        } else {
            let order = order_base + super::end_order::slot(None, self.control.end_phaser_seq);
            self.control.end_phaser_seq += 1;
            order
        };
        let slot = self.control.end_phasers.len();
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
        self.control.end_phasers.push(super::EndPhaser {
            body,
            env,
            package,
            unit: self.executing_unit_sym_for_module_load(),
            dead_keys,
            order,
            capture_seq: None,
            fixtures: None,
            unit_routines: Default::default(),
            import_chain: None,
        });
        self.control.main_end_slots.insert(end_index, slot);
    }

    /// Drop the END phasers `preregister_main_end_phasers` installed for the
    /// main compunit: a CHECK-time rewrite of `$*CU` made the unit run its
    /// rewritten tree, which installs its own (#17501).
    // Cost: O(e), e = registered END phasers.
    pub(crate) fn drop_main_end_phasers(&mut self) {
        let mut slots = Vec::new();
        self.control.main_end_slots.retain(|_, slot| {
            let is_main = self.control.end_phasers[*slot].order < super::end_order::RUNTIME;
            if is_main {
                slots.push(*slot);
            }
            !is_main
        });
        if slots.is_empty() {
            return;
        }
        slots.sort_unstable();
        for slot in self.control.main_end_slots.values_mut() {
            *slot -= slots.partition_point(|removed| *removed < *slot);
        }
        let mut index = 0;
        self.control.end_phasers.retain(|_| {
            let keep = slots.binary_search(&index).is_err();
            index += 1;
            keep
        });
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
            Some('%') => crate::value::Value::hash(ValueMap::default()),
            // Scalars are stored under their BARE name (no `$`), the same
            // convention `Stmt::VarDecl::name` uses — see `push_lexical`.
            _ => crate::value::Value::package(crate::symbol::wk::any()),
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
        let Some(slot) = end_index.and_then(|i| self.control.main_end_slots.get(&i).copied())
        else {
            return false;
        };
        let captured_env = self.env.clone();
        let package = self.current_package();
        let unit = self.executing_unit_sym_for_module_load();
        let mark = self.control.end_phaser_capture_seq;
        self.control.end_phaser_capture_seq += 1;
        let fixtures = self.frame_fixtures().cloned();
        let eval_capture = (self.control.end_phasers[slot].order >= super::end_order::RUNTIME)
            .then(|| (self.eval_unit_routines(), self.active_lexical_import_chain()));
        let phaser = &mut self.control.end_phasers[slot];
        phaser.fixtures = fixtures;
        phaser.env = captured_env;
        phaser.package = package;
        phaser.unit = unit;
        // A fresh capture supersedes whatever the previous one froze.
        phaser.dead_keys = crate::runtime::NameSet::default();
        phaser.capture_seq = Some(mark);
        if let Some((routines, imports)) = eval_capture {
            phaser.unit_routines = routines;
            phaser.import_chain = Some(imports);
        }
        true
    }

    /// The routines the running `EVAL` unit has declared so far, for an END it
    /// installs: the unit's routines are lexical to it and leave the registry
    /// with it, but its END phasers run at program exit and still call them.
    /// Empty outside an `EVAL` unit.
    // Cost: O(r), r = routines in the registry; only inside an `EVAL` unit.
    fn eval_unit_routines(
        &self,
    ) -> std::sync::Arc<Vec<(Symbol, std::sync::Arc<crate::ast::FunctionDef>)>> {
        let Some(base) = &self.module.eval_unit_functions_base else {
            return Default::default();
        };
        let added: Vec<(Symbol, std::sync::Arc<crate::ast::FunctionDef>)> = self
            .routines()
            .functions
            .iter()
            .filter(|(key, def)| {
                base.get(key)
                    .is_none_or(|old| !std::sync::Arc::ptr_eq(old, def))
            })
            .map(|(key, def)| (*key, def.clone()))
            .collect();
        std::sync::Arc::new(added)
    }

    /// Install `routines` that the registry lacks, for an END phaser's run.
    /// Returns the registry as it was, to be put back with
    /// `restore_routine_registry`, or `None` when nothing was added.
    // Cost: O(k), k = routines the phaser carries.
    pub(crate) fn reinstall_unit_routines(
        &mut self,
        routines: &[(Symbol, std::sync::Arc<crate::ast::FunctionDef>)],
    ) -> Option<crate::runtime::RoutineRegistrySnapshot> {
        let missing: Vec<&(Symbol, std::sync::Arc<crate::ast::FunctionDef>)> = {
            let registry = self.routines();
            routines
                .iter()
                .filter(|(key, _)| !registry.functions.contains_key(key))
                .collect()
        };
        if missing.is_empty() {
            return None;
        }
        let snapshot = self.snapshot_routine_registry();
        let keys: Vec<Symbol> = missing.iter().map(|(key, _)| *key).collect();
        for (key, def) in missing {
            self.routines_mut()
                .deref_mut_tables()
                .install_function(*key, def.clone());
        }
        self.invalidate_fn_resolution_for_keys(keys);
        Some(snapshot)
    }

    fn push_end_phaser_ordered(&mut self, body: Vec<Stmt>, order_base: u64, index: Option<u32>) {
        let captured_env = self.env.clone();
        let package = self.current_package();
        let order = match self.control.module_end_reservation.as_mut() {
            // A module whose `use` reserved its install position (#18183).
            Some((seq, k)) if order_base == super::end_order::MODULE => {
                let order = super::end_order::reserved_slot(*seq, *k);
                *k += 1;
                order
            }
            _ => order_base + super::end_order::slot(index, self.control.end_phaser_seq),
        };
        self.control.end_phaser_seq += 1;
        let mark = self.control.end_phaser_capture_seq;
        self.control.end_phaser_capture_seq += 1;
        self.control.end_phasers.push(super::EndPhaser {
            body,
            env: captured_env,
            package,
            unit: self.executing_unit_sym_for_module_load(),
            dead_keys: crate::runtime::NameSet::default(),
            order,
            capture_seq: Some(mark),
            fixtures: self.frame_fixtures().cloned(),
            unit_routines: self.eval_unit_routines(),
            import_chain: self
                .module
                .eval_unit_functions_base
                .is_some()
                .then(|| self.active_lexical_import_chain()),
        });
    }

    /// A mark of "how many END-phaser env captures have happened so far",
    /// taken on scope entry and handed back to [`Self::update_end_phaser_envs`] on
    /// scope exit to name the captures this scope is responsible for freezing.
    ///
    /// This replaced the old `end_phasers.len()` mark, which stopped meaning
    /// anything once every main-compunit END was installed up front: with
    /// pre-installation the vector no longer grows when a scope registers a
    /// phaser, it only re-captures.
    pub(crate) fn end_phaser_capture_mark(&self) -> u64 {
        self.control.end_phaser_capture_seq
    }

    /// True when at least one END phaser is registered.
    pub(crate) fn has_end_phasers(&self) -> bool {
        !self.control.end_phasers.is_empty()
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
            .control
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

    /// True when at least one of `keys` names a live (non-frozen) entry of some
    /// END phaser's captured env — i.e. when
    /// [`Self::update_end_phaser_envs_for_keys`] has anything at all to do.
    ///
    /// This is the cheap half of that update: it asks the same two membership
    /// questions but skips the third lookup and, crucially, lets the caller
    /// skip flattening the live env (`clone_env`, O(env) for a scoped frame)
    /// when the answer is no. A closure whose capture shares no name with any
    /// phaser — the overwhelmingly common case, and every case at all once a
    /// module with a wide export list has widened the capture — then costs two
    /// integer-keyed lookups per captured name instead of a whole-env flatten.
    pub(crate) fn end_phasers_watch_any(&self, keys: &[Symbol]) -> bool {
        self.control.end_phasers.iter().any(|phaser| {
            keys.iter()
                .any(|k| !phaser.dead_keys.contains(k) && phaser.env.contains_key_sym(*k))
        })
    }

    /// Update captured envs of ALL END phasers, but only for `keys`.  Used
    /// after closure calls to propagate changes to captured variables without
    /// overwriting unrelated variables.
    ///
    /// `keys` are names of the *calling closure's* own captured env, which only
    /// coincidentally share a name with a phaser's captured entry — they
    /// are not necessarily the same binding (a same-named `my` in a sibling
    /// scope is a common case: `{ my $a = 42; END { say $a } }; my $a = 0;
    /// callit { $a }` calls a closure that captured the SECOND `$a`, which must
    /// not clobber the phaser's captured FIRST `$a`). A key already in
    /// `phaser.dead_keys` (frozen at the moment its own declaring scope died —
    /// see `update_end_phaser_envs`) is the phaser's authoritative surviving
    /// binding for that name and must never be overwritten by an unrelated
    /// same-named capture from elsewhere.
    ///
    /// The names are interned [`Symbol`]s, never resolved back to strings. This
    /// runs on EVERY closure return once any END phaser exists, so both the
    /// size of `keys` and the per-key cost are on the hot path of any program
    /// that loaded a module registering an `END` — which `use Test` does.
    /// Resolving each name to a `String` and re-interning it three times over
    /// (`dead_keys`, the phaser env, the live env) made this ~30% of the hot
    /// loop of a program that merely had `use Test` at the top (#7565). The
    /// caller is what keeps `keys` small: it passes the names the call could
    /// actually have CHANGED, not the whole capture — see the collection of
    /// `end_refresh_keys` in `call_compiled_closure_in_unit`.
    ///
    /// **A write whose value is already there is skipped**, by binding identity
    /// rather than by `==`: re-storing the very `Value` the phaser env already
    /// holds cannot change what the phaser reads, and a capture widened by an
    /// import list is dominated by names (`$*IN`, `$*OUT`, `Any`, `?FILE`, the
    /// built-in dynamics) that no call ever rebinds. `same_binding` is the
    /// right test and `==` is not: it is O(1) where a deep `==` walks container
    /// contents, and a container mutated in place keeps its binding — which is
    /// exactly the case where the phaser's own entry is that same container and
    /// so already sees the mutation.
    pub(crate) fn update_end_phaser_envs_for_keys(&mut self, keys: &[Symbol], current_env: &Env) {
        for phaser in self.control.end_phasers.iter_mut() {
            for k in keys {
                if phaser.dead_keys.contains(k) {
                    continue;
                }
                if let Some(old) = phaser.env.get_sym(*k)
                    && let Some(v) = current_env.get_sym(*k)
                    && !old.same_binding(v)
                {
                    let v = v.clone();
                    phaser.env.insert_sym(*k, v);
                }
            }
        }
    }

    /// Register an END phaser site_id. Returns true if this is the first
    /// registration (phaser should be pushed), false if already registered.
    pub(crate) fn register_end_phaser_site(&mut self, site_id: u64) -> bool {
        self.control.end_phaser_sites.insert(site_id)
    }
}
