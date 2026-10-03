//! `SetVarType` / `SetVarTypeScoped` — declaration-time type-constraint
//! registration for `my TYPE $x` (and the typed `@`/`%` container forms).
use super::*;

impl Interpreter {
    /// Execute a `SetVarType` / `SetVarTypeScoped` / `SetVarTypeHoisted` op.
    /// `scoped` selects the env-only registration used for a scalar `my`/`state`
    /// lexically inside a routine (see `OpCode::SetVarTypeScoped`); the
    /// registration store is the ONLY difference between the first two ops — the
    /// Nil→type-object seeding and container tagging below are shared.
    ///
    /// `hoisted` marks the block-entry pre-registration emitted by
    /// `Compiler::hoist_typed_var_decls` (see `OpCode::SetVarTypeHoisted`): the
    /// declaration has not run yet, so anything currently bound to the name
    /// belongs to an enclosing scope and MUST NOT be written. It therefore only
    /// registers the constraint, and seeds the type object solely for a name
    /// that has no binding at all.
    // Cost: O(p), p = packages probed to qualify a user type name
    // (`resolve_type_in_current_package`, skipped for unshadowed core names); the type-object
    // seed path adds an O(L) by-name local update, L = locals of the chunk, only while the
    // variable is still unbound.
    pub(super) fn exec_set_var_type(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        name_idx: u32,
        tc_idx: u32,
        scoped: bool,
        hoisted: bool,
    ) -> Result<(), RuntimeError> {
        // Both operands are constant-pool `&str`s owned by `code`, which is not
        // borrowed from `self` — so they need no copy. Taking one anyway cost
        // two heap allocations (and two frees) on EVERY execution of a typed
        // declaration; this op was the single largest identified allocation
        // site on a `JSON::Fast.from-json` parse at 172,747 allocations per 100
        // records (#8830).
        let name: &str = Self::const_str(code, name_idx);
        // The name's symbol comes from the chunk's constant-pool table instead
        // of a `Symbol::intern` per use (this op interned it twice).
        let name_sym = code.const_sym(name_idx);
        let raw_constraint: &str = Self::const_str(code, tc_idx);
        let type_key = Interpreter::type_meta_key_for_sym(name_sym);
        self.save_type_meta_for_scope_exit(name, name_sym, type_key);
        // Empty constraint = CLEAR: an untyped expression-position
        // declaration dropping a stale same-named constraint (the
        // compiler never emits an empty string for a real type).
        if raw_constraint.is_empty() {
            self.vm_set_var_type_constraint(name, None);
            *ip += 1;
            return Ok(());
        }
        // Resolve type capture variables (e.g., `T` → `Int` when `::T`
        // was captured earlier in the signature). Through the `try_` form
        // (#8815's split) so the overwhelmingly common case -- a plain
        // constraint like `int` or `Str` with no capture, generic or package
        // alias to resolve -- borrows the constant-pool string instead of
        // allocating a copy of it to immediately compare and drop.
        //
        // A plain builtin name nothing shadows resolves to itself through every
        // step below; the per-site memo answers that without asking them
        // (see `type_decl_constraint_is_plain_builtin`).
        let plain_builtin =
            self.type_decl_constraint_is_plain_builtin(code, tc_idx, raw_constraint);
        let constraint: std::borrow::Cow<'_, str> = if plain_builtin {
            std::borrow::Cow::Borrowed(raw_constraint)
        } else {
            match loan_env!(self, try_resolved_type_capture_name(raw_constraint)) {
                Some(resolved) => std::borrow::Cow::Owned(resolved),
                None => std::borrow::Cow::Borrowed(raw_constraint),
            }
        };
        // Container metadata is later rendered as `Array[T]`/`Hash[T]`, so a
        // user type used unqualified inside a module must retain the same
        // package-qualified identity as the type checker.  Keeping the raw
        // spelling (`DataSlice`) made a typed array report `Array[DataSlice]`
        // even though its elements were instances of `Dan::DataSlice`.
        //
        // A core name that nothing shadows is skipped, and that is not merely
        // an optimization of a walk that would have returned `None`: this op
        // runs on EVERY execution of a typed declaration, and the walk it
        // guards ends in `resolve_lexical_type_key`, whose miss path is a
        // linear scan of every registry key. Without the guard, JSON::Fast's
        // 43 `my int`/`my str` declarations doubled bench-json-fast
        // (3.35G -> 6.91G simulated instructions, 22% of the run in `memcmp`
        // alone) — `news/2026-09/typed-decl-package-probe-regression.md`.
        //
        // Both probes are byte scans (`str_scan.rs`): `contains("::")` builds a
        // `StrSearcher` and `contains('[')` a `CharSearcher`, per execution, for
        // two fixed one- and two-byte needles.
        let constraint: std::borrow::Cow<'_, str> = if !plain_builtin
            && !crate::runtime::utils::has_double_colon(constraint.as_ref())
            && !crate::runtime::utils::has_bracket(constraint.as_ref())
            && !self.unshadowed_builtin_type_name(&constraint)
        {
            match self.resolve_type_in_current_package(&constraint) {
                Some(resolved) => std::borrow::Cow::Owned(resolved),
                None => constraint,
            }
        } else {
            constraint
        };
        // Clear stale atomic CAS state when an @-variable is
        // (re-)declared with a type constraint like atomicint. Not on the
        // hoist: the state belongs to whatever container is bound right now,
        // which is the enclosing scope's, not this declaration's.
        if !hoisted && name.starts_with('@') && constraint == "atomicint" {
            self.clear_atomic_array_state(name);
        }
        if scoped {
            let plain_meta = plain_builtin.then(|| code.constants[tc_idx as usize].clone());
            self.loan_env_for(|i| {
                i.set_var_type_constraint_routine_scoped(
                    name,
                    name_sym,
                    type_key,
                    &constraint,
                    plain_meta,
                )
            });
        } else {
            self.loan_env_for(|i| i.set_var_type_constraint_decl(name, name_sym, &constraint));
        }
        // For scalar variables, if the current value is Nil, set it to the type object.
        // Exception: if the constraint is "Nil", keep the value as Nil
        // (the Nil type object is Nil itself, not the Package "Nil").
        if !name.starts_with('@') && !name.starts_with('%') && constraint != "Nil" {
            // On the hoist an EXISTING binding is the enclosing scope's, and
            // seeding it would overwrite the outer variable's value for good
            // (the block-exit restore only puts the metadata back). Seed only a
            // name nothing has bound — the shape the hoist exists for.
            let current = self.env().get_sym(name_sym);
            let is_nil = if hoisted {
                current.is_none()
            } else {
                matches!(current.map(Value::view), Some(ValueView::Nil) | None)
            };
            // ... or the variable still holds a DEAD seed: a type object for a
            // name nothing has registered. `hoist_typed_var_decls` emits a
            // block-entry `SetVarType` for every top-level `my TYPE $x`, which
            // runs BEFORE the `class`/`role` statements of the same block, so
            // for a type declared inside a package (`module M { class C {…} }`,
            // and anything an `EVAL` compiles while a module's sub is running)
            // the constraint was not yet resolvable and the hoist seeded a bare,
            // never-registered `C` with no methods. That value is not a
            // legitimate one — no assignment can produce a type object for an
            // unknown type — so the declaration itself re-seeds it now that the
            // type exists. Without this the dead seed lives in `env` under the
            // sigil-stripped key a bareword `C` also reads, so `my C $C .= new`
            // (the fused form calls `.new` on the *bareword*) died with
            // "Unknown method ... new on C". Re-seeding an already-correct value
            // is a no-op: the seed is a pure function of the constraint.
            // `Any` — the reset a fresh declaration's `SetVarDynamic` leaves in
            // a loop body — is known by construction, which settles that
            // steady state without the probe.
            let is_dead_seed = !is_nil
                && matches!(
                    current.map(Value::view),
                    Some(ValueView::Package(p))
                        if p != crate::symbol::wk::any() && !self.type_name_is_known(&p.resolve())
                );
            if is_nil || is_dead_seed {
                let init_val = self.typed_scalar_nil_seed_value(name, &constraint);
                self.set_env_with_main_alias(name, init_val.clone());
                self.update_local_if_exists(code, name, &init_val);
            }
        } else if let Some(value) = (!hoisted)
            .then(|| self.get_env_with_main_alias(name))
            .flatten()
        {
            let info = crate::runtime::ContainerTypeInfo {
                value_type: loan_env!(self, var_type_constraint(name))
                    .unwrap_or_else(|| constraint.into_owned()),
                key_type: if name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(name))
                } else {
                    None
                },
                declared_type: None,
            };
            // Hashes embed metadata in `HashData`; write the tagged value
            // back (no-op Arc for array/instance side-table containers).
            // Tagging an object hash also re-keys it by `.WHICH`
            // (see `tag_container_metadata`).
            let tagged = self.tag_container_metadata(value, info);
            self.set_env_with_main_alias(name, tagged.clone());
            self.update_local_if_exists(code, name, &tagged);
        }
        *ip += 1;
        Ok(())
    }

    /// Whether the declaration constraint at constant `tc_idx` is a plain
    /// builtin type name that nothing shadows — one for which
    /// `exec_set_var_type`'s resolution steps (type-capture substitution,
    /// package and constant aliases, package qualification) all return the
    /// spelling unchanged.
    ///
    /// The steps that can say otherwise without a registry write are asked
    /// on every call, and are two loads in the common program: a bound `::T`
    /// capture (`any_type_capture_seen`, process-global latch) and a module
    /// package alias (`package_type_aliases`, empty unless one was imported).
    /// A constant alias cannot rename a known type (`type_alias_target`
    /// answers `None` for every `is_known_type_constraint` name before it looks
    /// at any binding). What remains — a user `subset`/`enum`/`role` declared
    /// under the builtin name — lives in the registry, so the answer is
    /// memoized per site for one registry write generation
    /// ([`crate::value::TypeDeclSiteCaches`]).
    // Cost: O(1) on a memo hit; a miss adds O(|name|) for the spelling checks
    // plus three registry probes.
    pub(super) fn type_decl_constraint_is_plain_builtin(
        &self,
        code: &CompiledCode,
        tc_idx: u32,
        constraint: &str,
    ) -> bool {
        if Self::any_type_capture_seen() || !self.package_type_aliases.is_empty() {
            return false;
        }
        let generation = self.registry_write_generation();
        let sites = code.constants.len();
        let idx = tc_idx as usize;
        if code.type_decl_sites.cached(sites, idx, generation) {
            return true;
        }
        let plain = crate::runtime::utils::is_known_type_constraint(constraint)
            && !constraint
                .bytes()
                .any(|b| matches!(b, b':' | b'(' | b'[' | b'{' | b' '))
            && self.unshadowed_builtin_type_name(constraint);
        if plain && self.registry_write_generation() == generation {
            code.type_decl_sites.remember(sites, idx, generation);
        }
        plain
    }

    /// Record this name's PRE-declaration env-scoped constraint metadata
    /// (`__mutsu_type::<name>`, `__mutsu_hash_key_type::<name>`) into the
    /// innermost branch/loop-body scope, so `pop_loop_local_scope` puts it back
    /// — or removes it — when that body exits.
    ///
    /// Without this, a typed declaration that SHADOWS an already-existing outer
    /// binding of the same name leaks its constraint onto the outer variable:
    /// `my $x; if True { my Str $x = "a" }; $x = 42` died with "expected Str".
    /// `exec_block_local_scope_op`'s exit cleanup (ADR-0042 slice 1 step 4)
    /// deliberately skips every name that already existed in `env` before the
    /// branch — those are `pop_loop_local_scope`'s job — so a shadowing
    /// declaration's metadata was never undone by anything; and loop bodies
    /// (`while`/`until`/C-style `loop`/`repeat`/`for`) have no branch-exit
    /// cleanup at all, so they leaked for fresh declarations too.
    ///
    /// The save must happen HERE, at the moment the constraint is about to be
    /// overwritten, and not where `exec_set_local_op` records a shadowed
    /// binding's value: the compiler emits the type-constraint op BEFORE the
    /// declaration's own `SetLocalDecl` store, so by the time the store runs the
    /// metadata has already been clobbered and there is nothing left to save.
    /// (ADR-0042 §10 records a prototype that hooked the store instead and was
    /// measured to have no effect — this ordering is why.)
    ///
    /// First write wins (`or_insert`), so a loop body that re-declares on every
    /// iteration still restores the value from before the loop, matching how the
    /// surrounding shadow-restore records a name once per scope.
    ///
    /// Only a `%` name can carry hash-key metadata
    /// (`Interpreter::may_carry_hash_key_meta`), so only it records that key.
    fn save_type_meta_for_scope_exit(&mut self, name: &str, name_sym: Symbol, type_key: Symbol) {
        if self.topic_state.loop_local_saved_env.is_empty() {
            return;
        }
        let hash_key = Interpreter::may_carry_hash_key_meta(name)
            .then(|| Interpreter::hash_key_meta_key_for_sym(name_sym));
        for key in std::iter::once(type_key).chain(hash_key) {
            // First write wins, so once this scope has recorded the key there
            // is nothing left to do — and in particular no reason to copy the
            // key's bytes to build the `entry()` argument, nor to clone the
            // env value that `or_insert` would drop again. A loop body that
            // re-declares on every iteration takes this exit on all but the
            // first, which is the shape the save exists for (#8898).
            if self
                .topic_state
                .loop_local_saved_env
                .last()
                .is_some_and(|scope| scope.contains_key(key.as_str()))
            {
                continue;
            }
            let prev = self.env().get_sym(key).cloned();
            if let Some(scope) = self.topic_state.loop_local_saved_env.last_mut() {
                scope.insert(key.as_str().to_string(), prev);
            }
        }
    }

    /// The value a Nil-valued typed scalar holds under constraint `constraint`:
    /// the nominal type object, except native types (zero/empty defaults) and
    /// parameterized roles (the ParametricRole type object, so `.WHAT`/`.raku`
    /// keep the type arguments). Used both by the declaration-time seeding
    /// above and by the SetLocal store path when a Nil is ASSIGNED to a typed
    /// scalar — the read paths deliberately do not consult env-scoped
    /// constraints for Nil→type-object conversion (a `= Nil` parameter default
    /// must stay Nil), so the stored value itself must carry the type object.
    pub(crate) fn typed_scalar_nil_seed_value(&mut self, name: &str, constraint: &str) -> Value {
        let base = loan_env!(self, var_type_constraint(name));
        self.typed_scalar_nil_seed_value_with_base(constraint, base)
    }

    /// [`Self::typed_scalar_nil_seed_value`] with the declared constraint
    /// supplied by the caller rather than read back by name — for a store
    /// whose constraint came from the cell it lands in (a routine's write to
    /// its own free variable), where the name-keyed entry may belong to an
    /// unrelated same-named variable of the calling scope (#10049).
    pub(crate) fn typed_scalar_nil_seed_value_with_base(
        &mut self,
        constraint: &str,
        base: Option<String>,
    ) -> Value {
        if crate::runtime::native_types::is_native_int_type(constraint) {
            Value::int(0)
        } else if matches!(constraint, "num" | "num32" | "num64") {
            Value::num(0.0)
        } else if constraint == "str" {
            Value::str(String::new())
        } else {
            // A parameterized role constraint (`my Cup of EggNog $mug` /
            // `my Cup[EggNog] $mug`) resolves to the ParametricRole type
            // object. The stored constraint metadata normalizes to the base
            // name, so probe the raw constraint here.
            let parametric = constraint
                .contains('[')
                .then(|| loan_env!(self, type_arg_value_from_name(constraint)));
            match parametric {
                Some(v) if matches!(v.view(), ValueView::ParametricRole { .. }) => v,
                _ => {
                    // The seeded package must carry the NOMINAL type name —
                    // smileys stripped (`my Int:_ $a` seeds `Int`, not
                    // `Int:_`) and coercion parens unwrapped — same as the
                    // read-path Nil→type-object conversion it replaces.
                    let base = base.unwrap_or_else(|| constraint.to_string());
                    let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&base));
                    Value::package(Symbol::intern(&nominal))
                }
            }
        }
    }

    /// The scalar type constraint a by-name (`SetGlobal`) store to `name` must
    /// satisfy, and whether it came from the target cell.
    ///
    /// A routine's write to one of its own free variables (a mainline lexical
    /// it captured, ADR-0024, or a file-scope lexical of its compunit,
    /// ADR-0039) lands in the shared cell `unit_lexical_slot` resolves, not in
    /// whatever `env` holds under that name — `env` is a child of the CALLER's
    /// env, so its `__mutsu_type::` entry can be a same-named typed `my` of the
    /// calling scope (#10049). The constraint belongs to the container
    /// (ADR-0042), so for such a store the cell's own `of` is authoritative,
    /// and an untyped cell means no constraint at all. Every other store keeps
    /// the name-keyed lane.
    // Cost: O(p + e), p = packages probed by `unit_lexical_slot`, e = env
    // chain depth for the name-keyed fallback.
    pub(crate) fn by_name_store_scalar_constraint(
        &self,
        name: &str,
        name_sym: Symbol,
        free_var_store: bool,
    ) -> (Option<String>, bool) {
        if free_var_store
            && let Some(slot) = self.unit_lexical_slot(name)
            && let ValueView::ContainerRef(cell) = slot.view()
        {
            let constraint = crate::value::lookup_cell_constraint(&cell)
                .filter(|c| c.element_of.is_none())
                .map(|c| c.ty);
            return (constraint, true);
        }
        (self.var_type_constraint_sym(name_sym), false)
    }
}
