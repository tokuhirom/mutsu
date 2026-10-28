use super::multi_rank::RankProfile;
use super::*;

impl Interpreter {
    /// Resolve `method_name` for `invocant`. The invocant must be passed: a
    /// candidate's `where` clause can read `self` / `$!attr` (CSS::Module's
    /// `multi method parse-property(Str:D $n where (%!coerce{.lc}:exists), ...)`),
    /// and matching without it records an `X::Syntax::NoSelf` for the call.
    pub(super) fn resolve_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: &Value,
    ) -> Option<MethodDef> {
        self.resolve_method_with_owner_invocant(class_name, method_name, arg_values, invocant)
            .map(|(_, def)| def)
    }

    pub(crate) fn method_args_match_for_invocant(
        &mut self,
        class_name: &str,
        def: &MethodDef,
        arg_values: &[Value],
        role_bindings: Option<&rustc_hash::FxHashMap<String, Value>>,
        invocant: Option<&Value>,
    ) -> bool {
        // Candidate matching is a *speculative* window: everything it binds
        // (role type params, the method's captured lexical scope, `self`, type
        // captures) is rolled back when it ends, and it can run for every
        // candidate of a multi. Bind into a scoped overlay rather than into the
        // frame's own env: a `saved_env` clone shares the env's `Arc`, so the
        // first write used to `make_mut`-deep-copy the whole map -- 888 entries
        // with Cro's module stack loaded, 547 times over a 20-frame HTTP/2 run,
        // and it is the single biggest source of env deep copies on that path.
        // The overlay starts empty, so the same writes are O(1) and the
        // rollback is dropping a tier.
        // A candidate of plain nominal positionals reads none of the window
        // below: match it on the argument types alone (#12942).
        if let Some(verdict) = self.match_candidate_by_type_alone(def, role_bindings, arg_values) {
            return verdict;
        }
        let saved_env = self.env.clone();
        self.env = crate::env::Env::scoped_child(std::mem::take(&mut self.env));
        // A `where` constraint (or the type check just below) can run user
        // code that dispatches a nested method call, which clears
        // `pending_call_arg_sources` on its own exit (see the doc comment at
        // its clear site in `vm_call_method_mut_ops.rs`) — scoped to THAT
        // call, but this is a single `self` field, so the clear also wiped
        // out the OUTER, still-in-flight call's arg-source names before its
        // own `is rw` parameter got to bind them. Symptom: an `is rw` Buf
        // parameter bound from a real lexical (`self.parse($in, ...)`) failed
        // with "expects a writable container" only when a SIBLING multi
        // candidate for the same call had a `where` clause invoking `.HOW`/`~~`
        // (ASN::BER's `Parser.parse` — its `enum-type where
        // $enum-type.HOW ~~ Metamodel::EnumHOW` candidate cleared the arg
        // sources meant for the `ASNSequence` candidate ultimately selected).
        // Save and restore here, in the same speculative window as `env`.
        let saved_arg_sources = self.pending_call_arg_sources.clone();
        // Candidate matching happens before the method frame is installed.
        // Anchor package-local aliases to the receiver's class for this
        // speculative window so a nested subset such as
        // `Data::StaticTable::Position` can be written as `Position` in the
        // declaring class's signature.
        let saved_package = self.current_package();
        self.set_current_package(class_name.to_string());
        // The passed-in class-level map goes first (it also carries nested
        // generic-class rename entries, not just type-param bindings — see
        // the identical comment in `call_compiled_method`,
        // `vm_method_dispatch.rs`); the candidate's own `role_param_bindings`
        // is then overlaid on top so a `where`/sub-signature match sees ITS
        // OWN `T` when the same role is composed twice with different type
        // args, instead of whichever composition's flat map happened to win.
        if let Some(bindings) = role_bindings {
            for (name, value) in bindings {
                self.env.insert(name.clone(), value.clone());
            }
        }
        if let Some(bindings) = def.role_param_bindings.as_deref() {
            for (name, value) in bindings {
                self.env.insert(name.clone(), value.clone());
            }
        }
        // A declared class method can close over the routine that registered
        // its class. Candidate matching evaluates `where` constraints before
        // the method call installs its bytecode environment, so make the
        // captured lexical scope available here as well.
        if let Some(captured) = &def.captured_env {
            let authoritative = captured.contains_key_sym(crate::symbol::Symbol::intern(
                "__mutsu_declared_method_capture",
            ));
            for (sym, value) in captured.iter() {
                if !sym.with_str(|name| {
                    matches!(
                        name,
                        "self" | "?CLASS" | "?ROLE" | "__mutsu_declared_method_capture"
                    )
                }) && (authoritative || !self.env.contains_key_sym(*sym))
                {
                    self.env.insert_sym(*sym, value.clone());
                }
            }
        }
        // Candidate matching runs user code (a `where` constraint, and the
        // default expression a `where` is checked against) before the method
        // call installs its own environment, so `self` -- and hence `$.attr` --
        // has to be reachable from here too. Without it a signature like
        // `multi method message(Str:D $c where { $_ eq 'INTM' } = $.classifier)`
        // could not be told apart from its sibling and the first candidate
        // declared always won. The whole block runs inside the
        // `saved_env`/restore window, so this binding is scoped to the match.
        if let Some(inv) = invocant {
            self.env.insert("self".to_string(), inv.clone());
        }
        for pd in &def.param_defs {
            if !pd.is_invocant {
                continue;
            }
            // `::?CLASS` / `::?ROLE` are pseudo-types (the current class), NOT
            // type captures — reading them as a capture named `?CLASS`/`?ROLE:U`
            // would both bind a bogus capture and skip the invocant check, so a
            // role's `multi method m(::?ROLE:U:)` / `(::?ROLE:D:)` pair became an
            // ambiguous call once composed into a class. Only a genuine `::T`
            // capture (not starting with `?`) binds here.
            if let Some(captured_name) = pd.captured_type_name()
                && !captured_name.starts_with('?')
            {
                self.bind_type_capture(captured_name, &Value::package(Symbol::intern(class_name)));
            }
            if let Some(constraint) = pd.type_constraint.as_deref() {
                // Check type constraint on the invocant (including :U/:D smileys).
                // Resolve the `::?CLASS`/`::?ROLE` pseudo-types to the actual
                // class name for matching. Pure type captures (e.g. ::T) don't
                // constrain the invocant.
                if let Some(inv) = invocant {
                    let resolved = constraint
                        .replace("::?CLASS", class_name)
                        .replace("::?ROLE", class_name);
                    let is_type_capture = crate::qualified::is_type_capture(&resolved);
                    if !is_type_capture && !self.type_matches_value(&resolved, inv) {
                        self.env = saved_env;
                        self.set_current_package(saved_package);
                        self.pending_call_arg_sources = saved_arg_sources;
                        return false;
                    }
                }
            }
            // `multi method m($self where { ... }:)`: the invocant's own `where`.
            if let Some(inv) = invocant
                && pd.where_constraint.is_some()
                && !self.invocant_where_accepts(pd, inv, Some(Symbol::intern(class_name)))
            {
                self.restore_env_preserving_dynamics(saved_env);
                self.set_current_package(saved_package);
                self.pending_call_arg_sources = saved_arg_sources;
                return false;
            }
        }
        // The `where` clauses and defaults of the signature were compiled in
        // the method's declaring compunit, so a type that unit imported (a
        // module's own role, ADR-11136) must resolve here whoever calls.
        let args_match = match def.source_file.as_deref() {
            Some(file) => self.with_candidate_unit(file, |this| {
                this.method_args_match(arg_values, &def.param_defs)
            }),
            None => self.method_args_match(arg_values, &def.param_defs),
        };
        if def.param_defs.iter().any(|p| p.where_constraint.is_some()) {
            // A `where` clause is user code: its writes to *dynamic* variables
            // are observable side effects (`multi method f($ where { $*checked
            // ~= "d1"; ... })`, A01-limits/misc.t) and must survive the
            // restore, exactly as on the sub-dispatch path.
            self.restore_env_preserving_dynamics(saved_env);
        } else {
            self.env = saved_env;
        }
        self.set_current_package(saved_package);
        self.pending_call_arg_sources = saved_arg_sources;
        args_match
    }

    /// Restore `self.env` to `saved` while keeping any *dynamic*-variable
    /// (`$*name`) writes made since — the observable side effects of user code
    /// (e.g. a `where` clause) run during a speculative dispatch match whose
    /// bindings are otherwise rolled back.
    ///
    /// The dynamic-name test reads the symbol's memoized
    /// [`crate::symbol::flags::DYNAMIC_VAR_ENV_KEY`] bit rather than resolving
    /// each key to a `&str` and rescanning its sigils. This scan runs over
    /// every env key TWICE per multi candidate whose signature carries a
    /// `where` clause (once for the clause, once for the signature match), so
    /// the per-key thread-local round trip `with_str` costs dominated a
    /// `where`-heavy dispatch: a callgrind run of `Crane.exists` (17 candidates,
    /// every one `where`-constrained) put this function at 25.8% of the whole
    /// program with `LocalKey::with` as its top leaf
    /// ([#7858](https://github.com/tokuhirom/mutsu/issues/7858)). The memoized
    /// bit answers it with one relaxed atomic load.
    ///
    /// The bit mirrors [`crate::env::is_dynamic_var_env_key`], which trims
    /// *every* sigil before looking for the `*` twigil — so `@*x` / `%*x` /
    /// `&*x` writes now survive the rollback alongside `$*x` and the sigil-less
    /// `*x`. They are dynamic variables in Raku too; the older hand-rolled test
    /// (`*` or `$*` prefix) simply missed them.
    pub(crate) fn restore_env_preserving_dynamics(&mut self, saved: crate::env::Env) {
        let dyn_writes: Vec<(crate::symbol::Symbol, Value)> = self
            .env
            .iter()
            .filter(|(k, v)| k.is_dynamic_var_env_key() && saved.get_sym(**k) != Some(*v))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        self.env = saved;
        for (k, v) in dyn_writes {
            self.env.insert_sym(k, v);
        }
    }

    pub(crate) fn resolve_method_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(Symbol, MethodDef)> {
        self.resolve_method_with_owner_impl(class_name, method_name, arg_values, None, None)
    }

    pub(crate) fn resolve_method_with_owner_invocant(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: &Value,
    ) -> Option<(Symbol, MethodDef)> {
        self.resolve_method_with_owner_impl(
            class_name,
            method_name,
            arg_values,
            Some(invocant),
            None,
        )
    }

    /// Same as [`Self::resolve_method_with_owner_invocant`], but restricts the
    /// MRO walk to `boundary_owner`'s class and everything below it (i.e. the
    /// receiver's own class down through `boundary_owner`, inclusive) —
    /// ancestors beyond `boundary_owner` are not visible, even though they
    /// would be reachable in an ordinary (non-proto) multi-method dispatch.
    ///
    /// ADR-0019 E9c-2: this is the direct-resolution replacement for an
    /// EXPLICIT `proto method`'s `{*}` redispatch (formerly a by-name
    /// re-entry into `call_method_with_values` bracketed by the ambient
    /// `proto_redispatch_boundary` field, which this parameter replaces). See
    /// `todo/tickets/explicit-child-proto-assumes-parent-candidates.md` for
    /// why the boundary exists at all: an explicit `proto method` on some
    /// class in the MRO starts a fresh candidate set that does NOT inherit an
    /// ancestor's candidates of the same name (the inverse — a proto in a
    /// PARENT governing candidates a child adds with no proto of its own —
    /// still works, because `boundary_owner` is then that ancestor and the
    /// whole tail from the receiver down through it stays in scope).
    pub(crate) fn resolve_method_within_boundary(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: Option<&Value>,
        boundary_owner: Option<Symbol>,
    ) -> Option<(Symbol, MethodDef)> {
        self.resolve_method_with_owner_impl(
            class_name,
            method_name,
            arg_values,
            invocant,
            boundary_owner,
        )
    }

    fn resolve_method_with_owner_impl(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: Option<&Value>,
        boundary_owner: Option<Symbol>,
    ) -> Option<(Symbol, MethodDef)> {
        self.dispatch.dispatch_ambiguous = false;
        // An object reblessed on another thread may carry a mixin type this
        // thread has not composed yet (#11456).
        self.adopt_published_mixin(class_name);
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro_full = self.class_mro(class_name);
        let truncate_at: Option<usize> =
            boundary_owner.and_then(|owner| mro_full.iter().position(|s| *s == owner));
        let mro: &[Symbol] = match truncate_at {
            Some(pos) => &mro_full[..=pos],
            None => &mro_full[..],
        };
        // Collect all matching multi candidates across the MRO, then pick the
        // most specific one by type hierarchy distance.
        let mut all_matches: Vec<(Symbol, MethodDef)> = Vec::new();
        // The visible multi candidates that did not bind: they still shape
        // rakudo's tiers (see `pick_method_winner`).
        let mut rivals: Vec<(Symbol, MethodDef)> = Vec::new();
        // Track whether a non-multi submethod was found on an ancestor
        // (submethods block MRO search for their class but not for
        // descendants).
        let mut submethod_blocks = false;
        // Only the levels that declare `method_name` at all can contribute a
        // candidate; the memo lists them so the walk is O(candidate levels),
        // not O(MRO depth) with a registry probe per level.
        let levels = self.method_candidate_levels(class_name, method_name);
        for &level in levels.iter() {
            let level = level as usize;
            if level >= mro.len() {
                break;
            }
            let cn = &mro[level];
            // Hoist the clone to a `let` so the registry read guard drops here,
            // before method_args_match_for_invocant re-enters (&mut self). An
            // `if let` scrutinee would otherwise keep the temporary guard alive
            // across the whole block.
            // A bare role's punned class is intentionally removed again after
            // construction, so an instance retained by an attribute can still
            // carry the role's class name while the canonical class method
            // table is absent. In that case the role table is the dispatch
            // source; ordinary composed roles keep their flattened class rows
            // and remain on the canonical path.
            let overloads = if cn.as_str() == class_name
                && !self.registry().classes.contains_key(cn.as_str())
            {
                self.registry()
                    .get_method_overloads_with_role_fallback(cn.as_str(), method_name)
            } else {
                self.registry()
                    .get_method_overloads(cn.as_str(), method_name)
            };
            if let Some(overloads) = overloads {
                let any_multi = overloads.iter().any(|d| d.is_multi);
                let mut first_visible_non_multi: Option<MethodDef> = None;
                // Check if all overloads are submethods on an ancestor class
                let all_submethods = overloads.iter().all(|d| d.is_my);
                let is_ancestor = cn.as_str() != class_name;
                // Single-visible-candidate fast return: with exactly one visible
                // non-multi candidate (and no multi collected from a more-derived
                // class), the outcome is invariant to the speculative argument
                // match — a successful match returns this def, and a failed match
                // falls through to the same def via `first_visible_non_multi`.
                // Skip the match (env snapshot + speculative binding) entirely.
                // That includes a `where` clause or a subset-typed parameter:
                // its predicate is user code, and running it here as well as in
                // the binder would make its side effects happen twice per call
                // (#10935). A sub-signature is still matched here: the binder does
                // not yet run the `where` clauses nested in a slurpy's
                // sub-signature, so this match is their only check (#10989).
                // This is the hot shape of every BUILD/TWEAK
                // construction dispatch.
                if !any_multi && all_matches.is_empty() {
                    let mut visible = overloads
                        .iter()
                        .filter(|d| !(d.is_private || (d.is_my && is_ancestor)));
                    if let Some(only) = visible.next()
                        && visible.next().is_none()
                        && only.param_defs.iter().all(|pd| pd.sub_signature.is_none())
                    {
                        return Some((*cn, only.clone()));
                    }
                }
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    // Submethods are NOT inherited: skip if defined on an
                    // ancestor class rather than the receiver's own class.
                    if def.is_my && is_ancestor {
                        continue;
                    }
                    if !any_multi && first_visible_non_multi.is_none() {
                        first_visible_non_multi = Some(def.clone());
                    }
                    let args_match = self.method_args_match_for_invocant(
                        class_name,
                        &def,
                        arg_values,
                        role_bindings.as_ref(),
                        invocant,
                    );
                    if args_match {
                        if !any_multi && all_matches.is_empty() {
                            // Non-multi: return the first match immediately,
                            // but only if no multi candidates were already
                            // collected from a child class in the MRO.
                            return Some((*cn, def));
                        }
                        all_matches.push((*cn, def));
                    } else if def.is_multi {
                        rivals.push((*cn, def));
                    }
                }
                // A non-multi submethod on an ancestor blocks the MRO search
                // (the method name exists but no candidate matched and
                // submethods are not inherited).
                if !any_multi && is_ancestor && all_submethods {
                    submethod_blocks = true;
                    continue;
                }
                // Method name is present on this class, but no candidate matched.
                // For non-multi methods, stop here — a subclass override
                // hides the parent's version.
                if !any_multi && all_matches.is_empty() {
                    return first_visible_non_multi.map(|def| (*cn, def));
                }
            }
        }
        let _ = submethod_blocks; // used for control flow above
        self.pick_method_winner(mro, arg_values, invocant, all_matches, rivals)
    }

    /// The type distance of a method candidate's parameters from the actual
    /// arguments, lower being narrower, with its per-parameter
    /// [`RankProfile`]: the walk multi-SUB dispatch ranks by
    /// ([`Self::params_type_distance_profile`]), over every parameter but the
    /// invocant, resolving a bare type name from the package that declared the
    /// method.
    // Cost: O(p) plus one hierarchy walk per positional parameter, p = parameters.
    pub(super) fn method_candidate_type_distance_profile(
        &self,
        args: &[Value],
        def: &MethodDef,
    ) -> (usize, RankProfile) {
        let owner = super::multi_type_walk::WalkOwner {
            package: def.lexical_package,
        };
        self.params_type_distance_profile(args, &Self::method_rank_params(def), owner)
    }

    pub(crate) fn constraint_base_for_distance(constraint: &str) -> &str {
        let s = if constraint.ends_with(":D") || constraint.ends_with(":U") {
            &constraint[..constraint.len() - 2]
        } else {
            constraint
        };
        s.split('(').next().unwrap_or(s)
    }

    /// Count the visible (non-private, non-ancestor-submethod) overloads of
    /// `method_name` across `class_name`'s MRO — the same visibility filter as
    /// `resolve_all_methods_with_owner`, but without cloning any overload Vec
    /// or running signature matching, and with an early-out at 2. Used to skip
    /// the remaining-candidates pass entirely for single-candidate submethod
    /// dispatch (BUILD/TWEAK on every construction).
    pub(crate) fn count_visible_method_candidates(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> usize {
        let mro = self.class_mro(class_name);
        let registry = self.registry();
        let mut count = 0usize;
        for cn in mro.iter() {
            let is_ancestor = cn.as_str() != class_name;
            let overloads =
                registry.get_method_overloads_with_role_fallback(cn.as_str(), method_name);
            if let Some(ovs) = overloads {
                count += ovs
                    .iter()
                    .filter(|d| !(d.is_private || (d.is_my && is_ancestor)))
                    .count();
                if count > 1 {
                    return count;
                }
            }
        }
        count
    }

    pub(crate) fn resolve_all_methods_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Vec<(Symbol, MethodDef)> {
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro = self.class_mro(class_name);
        let mut matches = Vec::new();
        for cn in mro.iter() {
            // An MRO entry may be a regular class or a punned role used as a
            // parent (`class Foo is R1 { ... }`). Role methods are stored in
            // `self.registry().roles`, so fall back there when the entry is not a class.
            // Single guard for both the class and role method tables; clone the
            // matched overload Vec out so the guard drops before re-entry below.
            let overloads = self
                .registry()
                .get_method_overloads_with_role_fallback(cn.as_str(), method_name);
            if let Some(overloads) = overloads {
                let is_ancestor = cn.as_str() != class_name;
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    // Submethods are NOT inherited
                    if def.is_my && is_ancestor {
                        continue;
                    }
                    if self.method_args_match_for_invocant(
                        class_name,
                        &def,
                        arg_values,
                        role_bindings.as_ref(),
                        None,
                    ) {
                        matches.push((*cn, def));
                    }
                }
            }
        }
        self.drop_flattened_role_duplicates(&mut matches);
        matches
    }

    /// A composed role is *flattened* into the class: its methods are copied in,
    /// tagged with `role_origin`. mutsu also keeps the role itself in the class's
    /// MRO, so a walk over every level finds the same method twice — which would
    /// make `.*`/`.+` call it twice and put a redundant link in the
    /// `nextsame`/`callsame` chain. (Rakudo does not keep the role in the MRO at
    /// all, which is why it never sees the duplicate.) Drop the role's own copy
    /// whenever a class level already carries the flattened one.
    ///
    /// A role's raw entry must ALSO be dropped when a class level shadows it
    /// with an independently-authored override of the same name — not just a
    /// flattened copy. `resolve_class_stub_requirements` (`registration.rs`)
    /// already removes the flattened copy from `class_def.methods` whenever the
    /// class provides its own same-signature method, so in that case `matches`
    /// never carries a `role_origin`-tagged entry at all and the `flattened`-set
    /// check above stays silent. Ground truth (raku, `t/role-shadowed-method-
    /// in-defer-chain.t`): a `does`-composed role method is NEVER a
    /// `nextsame`/`callsame` chain entry once a class overrides it by name and
    /// matching signature — whether that override arrived via flattening or was
    /// written directly in the class body. Compare by signature, not identity,
    /// so a role candidate with a genuinely different (narrower) signature
    /// keeps participating normally.
    ///
    /// This must NOT apply to a role used as a punned parent (`class Foo is
    /// R1`, `t/callsame-punned-role-and-hyper-infix-sub.t`): raku's own
    /// `.^mro` puts a punned role in the real ancestor chain as a genuine
    /// class (`(C2) (R2) (Any) (Mu)`, verified against Rakudo v2026.06),
    /// unlike a `does`-composed role, which never appears in `.^mro` at all
    /// (`(C1) (Any) (Mu)`) — so overriding a punned role's method is ordinary
    /// single-inheritance shadowing, and `nextsame` legitimately reaches the
    /// parent. `install_role_puns` (`registration_class_compose_body.rs`)
    /// gives a punned role a real (if method-empty) `registry().classes`
    /// entry, which is exactly the fact `owner_is_unpunned_role` below reads
    /// to tell the two shapes apart.
    pub(crate) fn drop_flattened_role_duplicates(&self, matches: &mut Vec<(Symbol, MethodDef)>) {
        let flattened: HashSet<&str> = matches
            .iter()
            .filter_map(|(_, def)| def.role_origin.as_deref())
            .collect();
        let registry = self.registry();
        let owner_is_unpunned_role = |owner: &str| {
            registry.roles.contains_key(owner) && !registry.classes.contains_key(owner)
        };
        let class_owned: Vec<MethodDef> = matches
            .iter()
            .filter(|(owner, def)| {
                def.role_origin.is_none() && !owner_is_unpunned_role(owner.as_str())
            })
            .map(|(_, def)| def.clone())
            .collect();
        let doomed: Vec<Symbol> = matches
            .iter()
            .filter(|(owner, def)| {
                flattened.contains(owner.as_str())
                    || (def.role_origin.is_none()
                        && owner_is_unpunned_role(owner.as_str())
                        && class_owned
                            .iter()
                            .any(|cm| Self::method_signatures_match(def, cm)))
            })
            .map(|(owner, _)| *owner)
            .collect();
        if doomed.is_empty() {
            return;
        }
        matches.retain(|(owner, _)| !doomed.contains(owner));
    }

    /// Resolve methods for .*/,+ dispatch: one resolution per MRO level.
    /// For multi methods, dispatches through the proto at each level.
    ///
    /// Returns `(matches, any_failed)`. `any_failed` is true only when a
    /// defining MRO level's own multi candidates don't cover this call's
    /// arguments — mirroring real Rakudo, where `.*name`/`.+name` invokes
    /// each level in order and raises the moment a level fails to resolve,
    /// WITHOUT undoing the side effects of levels that already ran
    /// successfully before it. The caller must still invoke every entry in
    /// `matches` (in order) before consulting `any_failed`, so that those
    /// earlier levels' side effects happen exactly as they would in Rakudo;
    /// only after that should it report a dispatch error instead of a
    /// successful result. A single level's own signature mismatch used to
    /// discard the WHOLE result (every other level's match too), turning a
    /// partial success into total silence — see
    /// news/2026-08/mro-level-any-failed-partial-match.md.
    pub(crate) fn resolve_methods_per_mro_level(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> (Vec<(Symbol, MethodDef)>, bool) {
        let mro = self.class_mro(class_name);
        let mut defining_levels: Vec<Symbol> = Vec::new();
        for cn in mro.iter() {
            let is_ancestor = cn.as_str() != class_name;
            if let Some(overloads) = self
                .registry()
                .get_method_overloads(cn.as_str(), method_name)
            {
                let has_visible = overloads
                    .iter()
                    .any(|d| !d.is_private && (!d.is_my || !is_ancestor));
                if has_visible {
                    defining_levels.push(*cn);
                }
            }
        }
        if defining_levels.is_empty() {
            return (Vec::new(), false);
        }
        let any_multi = defining_levels.iter().any(|cn| {
            self.registry()
                .get_method_overloads(cn.as_str(), method_name)
                .is_some_and(|ovs| ovs.iter().any(|d| d.is_multi))
        });
        if !any_multi {
            // Non-multi: use the standard resolution
            return (
                self.resolve_all_methods_with_owner(class_name, method_name, arg_values),
                false,
            );
        }
        // Multi methods: dispatch through proto at each level
        let mut matches = Vec::new();
        let mut any_failed = false;
        for cn in &defining_levels {
            if let Some(resolved) = self.resolve_method_with_owner_impl(
                cn.as_str(),
                method_name,
                arg_values,
                None,
                None,
            ) {
                matches.push(resolved);
            } else {
                any_failed = true;
            }
        }
        self.drop_flattened_role_duplicates(&mut matches);
        (matches, any_failed)
    }
}
