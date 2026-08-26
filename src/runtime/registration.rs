use super::methods_signature_errors::make_private_permission_error;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn validate_callable_param_return_redeclaration(
        param_defs: &[ParamDef],
    ) -> Result<(), RuntimeError> {
        for pd in param_defs {
            if pd.type_constraint.is_some()
                && pd
                    .code_signature
                    .as_ref()
                    .is_some_and(|(_, ret)| ret.is_some())
            {
                return Err(RuntimeError::new(
                    "X::Redeclaration: only one way of specifying sub-signature return type allowed",
                ));
            }
        }
        Ok(())
    }

    pub(super) fn is_stub_routine_body(body: &[Stmt]) -> bool {
        let filtered: Vec<_> = body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        filtered.len() == 1
            && matches!(
                filtered[0],
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
            )
    }

    fn is_stub_method_def(def: &MethodDef) -> bool {
        Self::is_stub_routine_body(&def.body)
    }

    /// Methods every object inherits from `Mu`/`Any` and therefore always
    /// responds to, so a role requirement of this name is satisfied even when
    /// the composing class does not define it (rakudo satisfies a `method new
    /// {...}` role stub with the inherited `Mu.new`). Mirrors the universal-set
    /// in `value_can_method`; kept to the object-protocol / coercion defaults
    /// that a bare class genuinely provides.
    fn is_universal_object_method(method_name: &str) -> bool {
        matches!(
            method_name,
            "new"
                | "bless"
                | "CREATE"
                | "clone"
                | "defined"
                | "Bool"
                | "so"
                | "not"
                | "gist"
                | "raku"
                | "perl"
                | "Str"
                | "item"
                | "self"
                | "sink"
                | "WHAT"
                | "WHICH"
                | "WHERE"
                | "HOW"
                | "WHY"
                | "VAR"
                | "DEFINITE"
                | "isa"
                | "does"
                | "can"
                | "ACCEPTS"
        )
    }

    /// Remove duplicate method candidates that are the *same* underlying
    /// definition reaching a class through multiple composition paths (a role
    /// diamond). Identity is the method body's `Arc` pointer plus its positional
    /// signature (see the body comment for why), keeping the first occurrence.
    /// Genuinely-distinct definitions own separate allocations and are preserved,
    /// so a same-name-different-role conflict is still detected.
    fn dedup_method_candidates(defs: &mut Vec<MethodDef>) {
        // A method reaching the class through multiple composition paths (a role
        // diamond) is the *same* `MethodDef` cloned once per path, so all its
        // clones share the same `body` Arc allocation. Deduplicate by that
        // pointer identity, paired with the positional signature: two
        // genuinely-distinct definitions own separate allocations and survive
        // (`multi method f(Int)` twice, or `::?ROLE:U:` vs `:D:` differing only by
        // invocant), and the same parametric role composed at two different type
        // arguments (`does R[Str] does R[Int]`) shares the body Arc but keeps a
        // distinct signature, so it survives too -- only a true diamond duplicate
        // (same body *and* same signature) is collapsed.
        let mut seen: std::collections::HashSet<(*const Vec<Stmt>, Vec<String>)> =
            std::collections::HashSet::new();
        defs.retain(|def| {
            seen.insert((
                std::sync::Arc::as_ptr(&def.body),
                Self::method_positional_signature(def),
            ))
        });
    }

    pub(super) fn method_positional_signature(def: &MethodDef) -> Vec<String> {
        def.param_defs
            .iter()
            // The invocant is implicit and not part of the positional signature
            // for role/stub conformance: a method whose only difference from a
            // stub is a typed invocant marker (`method !foo(A:D:)` vs the stub
            // `method !foo`) still satisfies it.
            .filter(|pd| !(pd.is_invocant || pd.traits.iter().any(|t| t == "invocant")))
            .filter(|pd| !(pd.named || (pd.slurpy && pd.name.starts_with('%'))))
            .map(|pd| {
                if pd.slurpy {
                    format!("*{}", pd.type_constraint.as_deref().unwrap_or("Any"))
                } else {
                    pd.type_constraint.as_deref().unwrap_or("Any").to_string()
                }
            })
            .collect()
    }

    pub(super) fn method_signatures_match(required: &MethodDef, candidate: &MethodDef) -> bool {
        // Delegation methods can satisfy any stub requirement since they
        // transparently forward all arguments to the delegate.
        if candidate.delegation.is_some() {
            return required.is_private == candidate.is_private;
        }
        Self::method_positional_signature(required) == Self::method_positional_signature(candidate)
            && required.is_private == candidate.is_private
    }

    fn stub_is_nullary(def: &MethodDef) -> bool {
        def.param_defs.iter().all(|pd| pd.named || pd.slurpy)
    }

    fn inherited_matching_method_count(
        &mut self,
        class_name: &str,
        method_name: &str,
        required: &MethodDef,
    ) -> usize {
        let mut count = 0usize;
        let mro = self.class_mro(class_name);
        for parent in mro.iter().skip(1) {
            // No user-code re-entry in this loop body (only static helpers), so a
            // let-bound guard is safe.
            let registry = self.registry();
            let Some(defs) = registry.user_method_overloads(parent.as_str(), method_name) else {
                continue;
            };
            for def in &defs {
                if Self::is_stub_method_def(def) {
                    continue;
                }
                if Self::method_signatures_match(required, def) {
                    count += 1;
                }
            }
        }
        count
    }

    /// True when any class in the MRO above `class_name` provides a concrete
    /// (non-stub) method of this name, regardless of signature — rakudo's
    /// name-based satisfaction of role requirements, applied to inheritance.
    fn inherited_any_concrete_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for parent in mro.iter().skip(1) {
            let registry = self.registry();
            let Some(defs) = registry.user_method_overloads(parent.as_str(), method_name) else {
                continue;
            };
            if defs.iter().any(|def| !Self::is_stub_method_def(def)) {
                return true;
            }
        }
        false
    }

    fn accessor_matches_stub(
        &mut self,
        class_name: &str,
        method_name: &str,
        required: &MethodDef,
    ) -> bool {
        if !Self::stub_is_nullary(required) {
            return false;
        }
        self.collect_class_attributes(class_name)
            .iter()
            .any(|a| a.is_public && a.name == method_name)
    }

    pub(super) fn resolve_class_stub_requirements(
        &mut self,
        class_name: &str,
    ) -> Result<(), RuntimeError> {
        // ADR-0019 F4c-9b: the registry's `method_entries` table is the sole
        // store now, so this function reads/writes it directly instead of an
        // in-flight `ClassDef` parameter.
        let method_names: Vec<String> = self
            .registry()
            .owner_method_names(class_name)
            .iter()
            .map(Symbol::resolve)
            .collect();
        for method_name in method_names {
            let Some(all_defs) = self
                .registry()
                .user_method_overloads(class_name, &method_name)
            else {
                continue;
            };
            let mut stubs = Vec::new();
            let mut concrete = Vec::new();
            for def in all_defs {
                // Only a stub that came FROM A COMPOSED ROLE is a required method
                // the class must implement. A stub declared directly in the class
                // body (`class A { method foo {...} }`, role_origin == None) is an
                // abstract method: the class definition succeeds and the stub stays
                // a callable method that dies ("Stub code executed") only if invoked
                // — it must NOT raise X::Role::Composition::Unimplemented. So keep
                // class-direct stubs as concrete; only role-origin stubs are
                // requirements.
                if Self::is_stub_method_def(&def) && def.role_origin.is_some() {
                    stubs.push(def);
                } else {
                    concrete.push(def);
                }
            }
            // A method can reach the class through multiple composition paths (a
            // diamond: `class does Selector` where `Selector does DBIConn`, and
            // DBIConn's method is also pulled in directly). The *same* underlying
            // definition then appears several times, each tagged with a different
            // intermediate `role_origin`. `dedup_method_candidates` collapses those
            // by the shared body `Arc` pointer + positional signature, so a
            // diamond-shared method counts once -- while genuinely distinct
            // same-named methods from different roles still collide.
            Self::dedup_method_candidates(&mut stubs);
            Self::dedup_method_candidates(&mut concrete);
            if !stubs.is_empty() {
                for required in &stubs {
                    let matching: Vec<&MethodDef> = concrete
                        .iter()
                        .filter(|candidate| Self::method_signatures_match(required, candidate))
                        .collect();
                    let local_matches = matching.len();
                    if local_matches > 1 {
                        // If the class itself provides a concrete method (role_origin is None),
                        // it resolves the conflict — no error needed.
                        let class_provides = matching.iter().any(|m| m.role_origin.is_none());
                        if !class_provides {
                            return Err(RuntimeError::new(format!(
                                "X::Role::Composition::Conflict: multiple candidates for required method '{}'",
                                method_name
                            )));
                        }
                    }
                    if local_matches == 0 {
                        // rakudo satisfies a role's required NON-MULTI method by
                        // NAME — the stub's signature is advisory, not enforced
                        // (`method f()` satisfies a stub `f(Int $x, Str $y -->
                        // Str)`). So any concrete same-named method in this class
                        // satisfies the requirement even when no candidate matches
                        // the stub's positional signature. Cro::HTTP::BodySerializers
                        // relies on this: the class implements the Cro::Core stub
                        // `serialize(Cro::Message, $body)` with a proto/multi set
                        // typed at the narrower Cro::HTTP::Message. A stubbed
                        // *multi* keeps per-candidate signature enforcement —
                        // `multi method a(Int) { ... }` is NOT satisfied by
                        // `multi method a(Str)` (S14-roles/stubs.t "Interface
                        // contract enforced on stubbed multi"). The exact-signature
                        // `matching` above still drives the multiple-candidates
                        // conflict check, unchanged.
                        if !required.is_multi && !concrete.is_empty() {
                            continue;
                        }
                        let inherited_matches = self.inherited_matching_method_count(
                            class_name,
                            &method_name,
                            required,
                        );
                        let accessor_match = usize::from(self.accessor_matches_stub(
                            class_name,
                            &method_name,
                            required,
                        ));
                        let total = inherited_matches + accessor_match;
                        if total == 0 && Self::is_universal_object_method(&method_name) {
                            // A role requirement is satisfied by NAME, and every
                            // object inherits a set of methods from `Mu`/`Any`
                            // (`new`, `bless`, `clone`, `gist`, `defined`, ...).
                            // rakudo composes fine when a role stub `method new
                            // {...}` is "implemented" only by the inherited
                            // `Mu.new`; the class need not redefine it. (Real
                            // dist: Tree::Binary::PrettyTree provides only
                            // `submethod BUILD`, relying on `Mu.new` to satisfy
                            // `Renderer`'s `method new {...}` requirement.)
                            continue;
                        }
                        if total == 0 {
                            // Same name-based rule for inherited methods: a parent
                            // class's concrete method of this name satisfies a
                            // NON-MULTI stub even when its signature differs.
                            if !required.is_multi
                                && self.inherited_any_concrete_method(class_name, &method_name)
                            {
                                continue;
                            }
                            // rakudo: "Method 'o' must be implemented by A
                            // because it is required by roles: C1, R1." — an
                            // X::Comp-flavored compile error naming every role
                            // that requires the method.
                            let mut role_names: Vec<String> = self
                                .registry()
                                .user_method_overloads(class_name, &method_name)
                                .map(|defs| {
                                    defs.iter()
                                        .filter(|d| {
                                            Self::is_stub_method_def(d) && d.role_origin.is_some()
                                        })
                                        .filter_map(|d| d.role_origin.clone())
                                        .collect()
                                })
                                .unwrap_or_default();
                            role_names.sort();
                            role_names.dedup();
                            return Err(RuntimeError::typed_msg(
                                "X::Comp::AdHoc",
                                format!(
                                    "Method '{}' must be implemented by {} because it is required by roles: {}.",
                                    method_name,
                                    class_name,
                                    role_names.join(", ")
                                ),
                            ));
                        }
                        if total > 1 {
                            return Err(RuntimeError::new(format!(
                                "X::Role::Composition::Conflict: multiple inherited candidates for required method '{}'",
                                method_name
                            )));
                        }
                    }
                }
            }
            // When a class provides a multi candidate matching a role candidate,
            // the class version takes priority — remove the role duplicate.
            let class_methods: Vec<MethodDef> = concrete
                .iter()
                .filter(|m| m.role_origin.is_none())
                .cloned()
                .collect();
            if !class_methods.is_empty() {
                concrete.retain(|m| {
                    if m.role_origin.is_none() {
                        return true; // keep class methods
                    }
                    // Keep role method only if no class method has matching signature
                    !class_methods
                        .iter()
                        .any(|cm| Self::method_signatures_match(m, cm))
                });
            }
            // ADR-0019 F4c-3: dual-write, see class_body_method_decl's own
            // comment in registration_class_body_method.rs. Safe even though
            // this loop can still return `Err` on a later `method_name`
            // (ADR-0019 F4c-9b: the registry is now the sole store, so there
            // is no `class_def` straggler to worry about): `finalize_class_
            // registration`'s only caller rolls back via `ClassRegSnapshot::
            // restore`, which always restores the full pre-attempt row set
            // for this owner via `restore_user_method_rows` -- self-
            // sufficient repair of any partial state a failed attempt left
            // behind, needing no `class_def`-derived re-sync.
            let owner = Symbol::intern(class_name);
            if concrete.is_empty() {
                if stubs.is_empty() {
                    continue; // nothing changed, skip update
                }
                self.registry_mut()
                    .remove_user_methods(owner, Symbol::intern(&method_name));
            } else {
                self.registry_mut()
                    .set_user_methods(owner, Symbol::intern(&method_name), concrete);
            }
        }
        Ok(())
    }

    pub(super) fn validate_private_access_in_stmts(
        &self,
        caller_class: &str,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.validate_private_access_in_stmt(caller_class, stmt)?;
        }
        Ok(())
    }

    fn validate_private_access_in_stmt(
        &self,
        caller_class: &str,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) => {
                self.validate_private_access_in_expr(caller_class, e)?
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                self.validate_private_access_in_expr(caller_class, expr)?
            }
            Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for e in exprs {
                    self.validate_private_access_in_expr(caller_class, e)?;
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                self.validate_private_access_in_expr(caller_class, cond)?;
                self.validate_private_access_in_stmts(caller_class, then_branch)?;
                self.validate_private_access_in_stmts(caller_class, else_branch)?;
            }
            Stmt::While { cond, body, .. } => {
                self.validate_private_access_in_expr(caller_class, cond)?;
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::For { iterable, body, .. } => {
                self.validate_private_access_in_expr(caller_class, iterable)?;
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => {
                if let Some(init) = init.as_ref() {
                    self.validate_private_access_in_stmt(caller_class, init)?;
                }
                if let Some(cond) = cond.as_ref() {
                    self.validate_private_access_in_expr(caller_class, cond)?;
                }
                if let Some(step) = step.as_ref() {
                    self.validate_private_access_in_expr(caller_class, step)?;
                }
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::Block(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::RoleDecl { body, .. }
            | Stmt::SubDecl { body, .. }
            | Stmt::TokenDecl { body, .. }
            | Stmt::RuleDecl { body, .. }
            | Stmt::ProtoDecl { body, .. }
            | Stmt::Package { body, .. }
            | Stmt::React { body }
            | Stmt::When { body, .. }
            | Stmt::Given { body, .. }
            | Stmt::Phaser { body, .. }
            | Stmt::Subtest { body, .. } => {
                self.validate_private_access_in_stmts(caller_class, body)?
            }
            Stmt::Whenever { supply, body, .. } => {
                self.validate_private_access_in_expr(caller_class, supply)?;
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::MethodDecl { body, .. } => {
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::TempMethodAssign {
                method_args, value, ..
            } => {
                for e in method_args {
                    self.validate_private_access_in_expr(caller_class, e)?;
                }
                self.validate_private_access_in_expr(caller_class, value)?;
            }
            Stmt::Let { index, value, .. } => {
                if let Some(index) = index.as_ref() {
                    self.validate_private_access_in_expr(caller_class, index)?;
                }
                if let Some(value) = value.as_ref() {
                    self.validate_private_access_in_expr(caller_class, value)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Resolve a (possibly short) class name written in source — the owner of
    /// a qualified private call (`$o!Owner::meth`), or a `trusts` target — to
    /// its canonical registered form. Walks `context_class`'s enclosing
    /// package chain first (mirroring ordinary bareword type resolution: a
    /// name written inside `module Outer::Inner { class Renderer {...} }`
    /// resolves relative to `Outer::Inner`), then falls back to a direct
    /// global lookup for an already-qualified or genuine top-level name.
    /// Returns `short` unchanged when nothing resolves — an unresolvable
    /// name should still fail the caller's check rather than be silently
    /// accepted.
    pub(super) fn resolve_private_class_name(&self, context_class: &str, short: &str) -> String {
        // A lexical class (`my class Jar::Cookie { ... }`) registers under a
        // mangled storage name (ADR-0047 P1: `Foo\u{0}<decl-id>`) while `env`
        // binds the name written in source to it -- the same alias ordinary
        // bareword/qualified-method resolution follows. Without this, a
        // qualified private call whose owner is a lexical class (`trusts` /
        // `$o!Owner::meth`) canonicalizes to the dead bare name, which is
        // never a key in `class_trusts`, so the owner's `trusts` never
        // matches and every such call is wrongly denied.
        let remapped = self.lexical_env_remap_name(short);
        if remapped != short {
            return remapped;
        }
        let via_chain = self.resolve_type_name_for_owner(context_class, short.to_string());
        if via_chain != short {
            return via_chain;
        }
        if self.has_type_direct(short) {
            return short.to_string();
        }
        via_chain
    }

    /// Whether `caller_class` may access a private method whose owner is
    /// already the *canonical* registered class name `canonical_owner`
    /// (e.g. the owner resolved from a matched method, or a name already run
    /// through `resolve_private_class_name`): either they are the same
    /// class, or `canonical_owner` declared `trusts` on `caller_class`. Each
    /// `trusts` entry is itself canonicalized against `canonical_owner`'s
    /// package chain before comparing, so `trusts B;` written inside a
    /// `module` matches the module-qualified `B`, not just the literal
    /// source text `"B"`.
    pub(super) fn private_owner_trusts_caller(
        &self,
        caller_class: Option<&str>,
        canonical_owner: &str,
    ) -> bool {
        let Some(caller_class) = caller_class else {
            return false;
        };
        canonical_owner == caller_class
            || self
                .registry()
                .class_trusts
                .get(canonical_owner)
                .is_some_and(|trusted| {
                    trusted.iter().any(|t| t == caller_class)
                        || trusted.iter().any(|t| {
                            self.resolve_private_class_name(canonical_owner, t) == caller_class
                        })
                })
    }

    /// Resolve a qualified private call's source-written owner
    /// (`$o!Owner::meth`) to its canonical registered class relative to
    /// `caller_class`'s package chain, then check whether that owner trusts
    /// the caller. Returns `(canonical_owner, trusted)` — callers should
    /// report the canonical name in a permission-denied error, not the raw
    /// short name that was written in source.
    pub(super) fn resolve_and_check_private_owner(
        &self,
        caller_class: Option<&str>,
        owner_class: &str,
    ) -> (String, bool) {
        self.resolve_and_check_private_owner_on(caller_class, owner_class, None)
    }

    /// [`Self::resolve_and_check_private_owner`] with the invocant's own class
    /// available as a second resolution source.
    ///
    /// The lexical resolution above is right for a top-level or `our`-scoped
    /// class, but blind to a `my class` declared inside ANOTHER class's body:
    ///
    /// ```raku
    /// class A {
    ///     my class B { trusts A; method !p() { ... } }
    ///     method go { B.new()!B::p() }
    /// }
    /// ```
    ///
    /// `B` registers under the mangled lexical storage name
    /// `A::B\u{0}<decl-id>` (ADR-0047 P1), and by the time `A.go` runs the bare
    /// name `B` is no longer bound in the env `resolve_private_class_name`
    /// consults, so the owner canonicalized to the dead bare name `B` — never a
    /// key in `class_trusts`, so `B`'s own `trusts A` never matched and every
    /// such call was wrongly denied.
    ///
    /// The invocant settles it: `Owner` has to name a type in the invocant's
    /// own MRO for `$o!Owner::meth` to resolve at all. So when the lexically
    /// resolved name is absent from that MRO, match the name as written
    /// against each MRO entry's user-facing spelling (mangling stripped),
    /// accepting a full-name match or a trailing `::Owner` segment.
    pub(super) fn resolve_and_check_private_owner_on(
        &self,
        caller_class: Option<&str>,
        owner_class: &str,
        invocant_class: Option<&str>,
    ) -> (String, bool) {
        let mut canonical_owner = match caller_class {
            Some(c) => self.resolve_private_class_name(c, owner_class),
            None => owner_class.to_string(),
        };
        if let Some(invocant_class) = invocant_class
            && let Some(mro) = self.registry().class_mro_cached(invocant_class)
        {
            let mro: Vec<String> = mro.iter().map(crate::symbol::Symbol::resolve).collect();
            if !mro.contains(&canonical_owner) {
                let suffix = format!("::{owner_class}");
                if let Some(found) = mro.iter().find(|entry| {
                    let shown = crate::value::user_facing_type_name(entry);
                    shown == owner_class || shown.ends_with(&suffix)
                }) {
                    canonical_owner = found.clone();
                }
            }
        }
        let trusted = self.private_owner_trusts_caller(caller_class, &canonical_owner);
        (canonical_owner, trusted)
    }

    fn validate_private_access_in_expr(
        &self,
        caller_class: &str,
        expr: &Expr,
    ) -> Result<(), RuntimeError> {
        match expr {
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted: _,
            } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
                if *modifier == Some('!')
                    // Split at the LAST `::`: the owner class of a qualified private
                    // call may itself be a nested name (`$c!Cookie::Jar::Cookie::match`
                    // is owner `Cookie::Jar::Cookie`, not `Cookie::Jar`).
                    && let Some((owner_class, method_name)) = name.resolve().rsplit_once("::")
                {
                    // `owner_class` is the short name as written in source
                    // (`Renderer`), while `caller_class` is always the fully
                    // qualified registered name (`Outer::Inner::Renderer`).
                    // Canonicalize before comparing, the same way an ordinary
                    // bareword type reference resolves against its enclosing
                    // package chain — otherwise a perfectly legal self-call
                    // written from inside a `module` false-positives here.
                    let (canonical_owner, trusted) =
                        self.resolve_and_check_private_owner(Some(caller_class), owner_class);
                    if !trusted {
                        return Err(make_private_permission_error(
                            method_name,
                            &canonical_owner,
                            caller_class,
                        ));
                    }
                }
            }
            Expr::HyperMethodCall { target, args, .. } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::HyperMethodCallDynamic {
                target,
                name_expr,
                args,
                ..
            } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                self.validate_private_access_in_expr(caller_class, name_expr)?;
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::Call { args, .. }
            | Expr::UserRoutineCall { args, .. }
            | Expr::ArrayLiteral(args)
            | Expr::BracketArray(args, _)
            | Expr::CaptureLiteral(args)
            | Expr::StringInterpolation(args) => {
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::Reduction { expr, .. } => {
                self.validate_private_access_in_expr(caller_class, expr)?;
            }
            Expr::Binary { left, right, .. }
            | Expr::MetaOp { left, right, .. }
            | Expr::HyperOp { left, right, .. } => {
                self.validate_private_access_in_expr(caller_class, left)?;
                self.validate_private_access_in_expr(caller_class, right)?;
            }
            // `todo/tickets/chained-compare-ast-node.md`: `$a !private-method
            // < b < c`-shaped chains can hold a private-method access in any
            // operand, same as `Binary` above.
            Expr::ChainedCompare { operands, .. } => {
                for o in operands {
                    self.validate_private_access_in_expr(caller_class, o)?;
                }
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.validate_private_access_in_expr(caller_class, cond)?;
                self.validate_private_access_in_expr(caller_class, then_expr)?;
                self.validate_private_access_in_expr(caller_class, else_expr)?;
            }
            Expr::Index { target, index, .. } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                self.validate_private_access_in_expr(caller_class, index)?;
            }
            Expr::IndexAssign {
                target,
                index,
                value,
                ..
            } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                self.validate_private_access_in_expr(caller_class, index)?;
                self.validate_private_access_in_expr(caller_class, value)?;
            }
            Expr::AssignExpr { expr, .. } => {
                self.validate_private_access_in_expr(caller_class, expr)?
            }
            Expr::DoBlock { body, .. }
            | Expr::Block(body)
            | Expr::Gather(body)
            | Expr::AnonSub { body, .. }
            | Expr::AnonSubParams { body, .. }
            | Expr::Lambda { body, .. } => {
                self.validate_private_access_in_stmts(caller_class, body)?
            }
            // ADR-0033: an un-expanded WhateverCurry body can still contain a
            // private-method access (`* !private-method`).
            Expr::WhateverCurry(inner) => {
                self.validate_private_access_in_expr(caller_class, inner)?
            }
            Expr::Try { body: _, catch } => {
                // Skip private access validation inside try blocks — unauthorized
                // private access will produce a runtime error that try can catch.
                if let Some(catch) = catch.as_ref() {
                    self.validate_private_access_in_stmts(caller_class, catch)?;
                }
            }
            Expr::DoStmt(stmt) => self.validate_private_access_in_stmt(caller_class, stmt)?,
            Expr::CallOn { target, args } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::InfixFunc { left, right, .. } => {
                self.validate_private_access_in_expr(caller_class, left)?;
                for arg in right {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::Exists { target, arg, .. } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                if let Some(a) = arg {
                    self.validate_private_access_in_expr(caller_class, a)?;
                }
            }
            Expr::ZenSlice(inner) => {
                self.validate_private_access_in_expr(caller_class, inner)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Validate that all `self!method()` calls in the class body reference
    /// private methods that actually exist on the class (compile-time check).
    pub(super) fn validate_private_method_existence(
        &self,
        class_name: &str,
    ) -> Result<(), RuntimeError> {
        let class_def = match self.registry().classes.get(class_name) {
            Some(cd) => cd.clone(),
            None => return Ok(()),
        };
        // ADR-0019 F4c-1: enumerate via the canonical reverse index instead
        // of `class_def.methods.values()` (zero-mismatch shadow-checked
        // across the full local `t/` suite before this cutover).
        let registry = self.registry();
        for method_name in registry.owner_method_names(class_name) {
            let method_name = method_name.resolve();
            let Some(overloads) = registry.user_method_overloads(class_name, &method_name) else {
                continue;
            };
            for method_def in &overloads {
                self.check_private_calls_exist(class_name, &class_def, &method_def.body)?;
            }
        }
        Ok(())
    }

    /// Validate `self!method()` private calls in freshly compiled statements
    /// (e.g. an EVAL'd string) against the class of the lexical `self` in scope.
    /// Raku resolves private method dispatch at compile time, so a call to a
    /// nonexistent private method is an error even when a preceding `return`
    /// would short-circuit it at runtime — this reproduces that for EVAL bodies
    /// running inside a method.
    pub(crate) fn validate_private_calls_against_self(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let class_name = match self.env.get("self").map(Value::view) {
            Some(ValueView::Instance { class_name, .. }) => class_name.resolve(),
            _ => return Ok(()),
        };
        let class_def = match self.registry().classes.get(&class_name) {
            Some(cd) => cd.clone(),
            None => return Ok(()),
        };
        self.check_private_calls_exist(&class_name, &class_def, stmts)
    }

    fn check_private_calls_exist(
        &self,
        class_name: &str,
        class_def: &ClassDef,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.check_private_calls_exist_stmt(class_name, class_def, stmt)?;
        }
        Ok(())
    }

    fn check_private_calls_exist_stmt(
        &self,
        class_name: &str,
        class_def: &ClassDef,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) => {
                self.check_private_calls_exist_expr(class_name, class_def, e)?;
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, expr)?;
            }
            Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for e in exprs {
                    self.check_private_calls_exist_expr(class_name, class_def, e)?;
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                self.check_private_calls_exist_expr(class_name, class_def, cond)?;
                self.check_private_calls_exist(class_name, class_def, then_branch)?;
                self.check_private_calls_exist(class_name, class_def, else_branch)?;
            }
            Stmt::While { cond, body, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, cond)?;
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            Stmt::For { iterable, body, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, iterable)?;
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            Stmt::Block(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::When { body, .. }
            | Stmt::Given { body, .. }
            | Stmt::Phaser { body, .. } => {
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn check_private_calls_exist_expr(
        &self,
        class_name: &str,
        class_def: &ClassDef,
        expr: &Expr,
    ) -> Result<(), RuntimeError> {
        match expr {
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                ..
            } => {
                self.check_private_calls_exist_expr(class_name, class_def, target)?;
                for arg in args {
                    self.check_private_calls_exist_expr(class_name, class_def, arg)?;
                }
                // Check self!method() calls
                if *modifier == Some('!')
                    && matches!(target.as_ref(), Expr::BareWord(w) if w == "self")
                {
                    let method_name = name.resolve();
                    // Skip owner-qualified calls (e.g., Class::method)
                    if !method_name.contains("::") {
                        let has_method = self
                            .registry()
                            .user_method_overloads(class_name, &method_name)
                            .is_some_and(|overloads| overloads.iter().any(|md| md.is_private));
                        if !has_method {
                            return Err(
                                super::methods_signature_errors::make_method_not_found_error(
                                    &method_name,
                                    class_name,
                                    true,
                                ),
                            );
                        }
                    }
                }
            }
            Expr::Binary { left, right, .. }
            | Expr::MetaOp { left, right, .. }
            | Expr::HyperOp { left, right, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, left)?;
                self.check_private_calls_exist_expr(class_name, class_def, right)?;
            }
            // `todo/tickets/chained-compare-ast-node.md`: same as `Binary`.
            Expr::ChainedCompare { operands, .. } => {
                for o in operands {
                    self.check_private_calls_exist_expr(class_name, class_def, o)?;
                }
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::Reduction { expr, .. }
            | Expr::AssignExpr { expr, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, expr)?;
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.check_private_calls_exist_expr(class_name, class_def, cond)?;
                self.check_private_calls_exist_expr(class_name, class_def, then_expr)?;
                self.check_private_calls_exist_expr(class_name, class_def, else_expr)?;
            }
            Expr::Index { target, index, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, target)?;
                self.check_private_calls_exist_expr(class_name, class_def, index)?;
            }
            Expr::Call { args, .. }
            | Expr::UserRoutineCall { args, .. }
            | Expr::ArrayLiteral(args)
            | Expr::BracketArray(args, _)
            | Expr::StringInterpolation(args) => {
                for arg in args {
                    self.check_private_calls_exist_expr(class_name, class_def, arg)?;
                }
            }
            Expr::Block(body)
            | Expr::AnonSub { body, .. }
            | Expr::AnonSubParams { body, .. }
            | Expr::Lambda { body, .. }
            | Expr::Gather(body) => {
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            Expr::DoBlock { body, .. } => {
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            // ADR-0033: an un-expanded WhateverCurry body can still contain a
            // private-method call (`* !method`).
            Expr::WhateverCurry(inner) => {
                self.check_private_calls_exist_expr(class_name, class_def, inner)?
            }
            Expr::DoStmt(stmt) => {
                self.check_private_calls_exist_stmt(class_name, class_def, stmt)?;
            }
            Expr::Try { body, catch } => {
                self.check_private_calls_exist(class_name, class_def, body)?;
                if let Some(catch) = catch.as_ref() {
                    self.check_private_calls_exist(class_name, class_def, catch)?;
                }
            }
            Expr::CallOn { target, args } => {
                self.check_private_calls_exist_expr(class_name, class_def, target)?;
                for arg in args {
                    self.check_private_calls_exist_expr(class_name, class_def, arg)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(crate) fn has_function(&self, name: &str) -> bool {
        self.has_declared_function(name)
    }

    pub(crate) fn has_declared_function(&self, name: &str) -> bool {
        self.bare_name_packages()
            .iter()
            .any(|pkg| self.registry().has_declared_function(pkg, name))
    }

    pub(crate) fn is_implicit_zero_arg_builtin(name: &str) -> bool {
        matches!(name, "dir" | "lines" | "hash")
    }

    /// Check if a multi-dispatched function with the given name exists (any arity).
    pub(crate) fn has_multi_function(&self, name: &str) -> bool {
        self.registry()
            .has_multi_function(&self.bare_name_packages(), name)
    }

    /// Check if a user-defined function with the given name can accept the
    /// given args (arity + type check). Used to decide whether a user-defined
    /// sub should shadow a same-named builtin.
    /// Whether a declaration of `name` should beat mutsu's native TAP provider
    /// for this call. Shared by the two dispatch paths into that provider
    /// (`exec_call` and `call_function_fallback`) so they agree.
    ///
    /// The rule is the one from the qualified-call guard: decide on whether a
    /// *declaration* exists, not on whether the name is a builtin. `skip` is the
    /// single exception, because it is both a Test directive and a Raku list
    /// routine — a user `multi skip($n, +values)` accepts `skip 'reason', 2` on
    /// signature alone, so the name needs the same shape-based disambiguation
    /// the three `skip` dispatch sites already apply
    /// (`t/skip-user-multi-shadows-test.t`).
    ///
    /// The name set is the *wide* one (`is_test_function_name`), not just the
    /// `Test` module's own exports: roast's `Test::Util` / `Test::Tap` helpers
    /// (`is_run`, `doesn't-hang`, `tap-ok`, …) really are loaded from source, so
    /// the routine the file imported must win over the native provider. Keeping
    /// the native handlers only as the fallback for a file that calls a helper
    /// *without* loading its module is what retires those two rung-3 providers
    /// (`todo/tickets/retire-native-test-util-overrides.md`).
    pub(crate) fn user_test_decl_beats_native(&mut self, name: &str, args: &[Value]) -> bool {
        if !Self::is_test_function_name(name) {
            return false;
        }
        if name == "skip" && !Self::skip_call_is_list_skip(args) {
            return false;
        }
        self.user_function_matches_call(name, args)
    }

    pub(crate) fn user_function_matches_call(&mut self, name: &str, args: &[Value]) -> bool {
        let has_fn = self.has_declared_function_cached(name);
        let has_multi = self.has_multi_function_cached(name);
        if !has_fn && !has_multi {
            return false;
        }
        let def = self.resolve_function_with_types(name, args);
        let Some(def) = def else {
            return false;
        };
        self.args_match_param_types(args, &def.param_defs)
    }

    /// A routine whose signature already pins the return value (`--> Nil`,
    /// `--> 42`, `--> "foo"`) may not also `return` an argument. Raku rejects
    /// this at compile time with an X::Comp::AdHoc carrying the offending value
    /// in its `payload`. `spec` is the verbatim return-type source (e.g. `Nil`,
    /// `42`, `"foo"`).
    pub(super) fn malformed_return_value_compile_error(spec: &str) -> RuntimeError {
        let message = format!(
            "No return arguments allowed when return value {} is already specified in the signature",
            spec.trim()
        );
        let mut err = RuntimeError::new(&message);
        err.set_code(Some(crate::value::RuntimeErrorCode::ParseGeneric));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        attrs.insert("payload".to_string(), Value::str(message));
        // X::Comp::AdHoc does both X::Comp and X::AdHoc in rakudo, so this
        // satisfies both `~~ X::Comp` (misc2.t:326-329) and `~~ X::AdHoc`
        // (S06-signature/definite-return.t "even a Failure").
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Comp::AdHoc"),
            attrs,
        )));
        err
    }

    /// Returns true if the statement list contains a `return` statement
    /// (including a bare `return` with no value), descending into nested
    /// control-flow blocks but not into nested routine declarations.
    ///
    /// Used to decide whether a `.map` callback must be evaluated lazily: a
    /// `return` inside a map block targets the lexically enclosing routine, so
    /// the callback must be deferred until the Seq is forced to get the correct
    /// out-of-dynamic-scope semantics. Plain map blocks (the overwhelming
    /// majority) keep eager evaluation.
    pub(crate) fn body_contains_return(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            match stmt {
                Stmt::Return(_) => return true,
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if Self::body_contains_return(then_branch)
                        || Self::body_contains_return(else_branch)
                    {
                        return true;
                    }
                }
                Stmt::While { body, .. }
                | Stmt::React { body }
                | Stmt::SyntheticBlock(body)
                | Stmt::Block(body)
                | Stmt::Subtest { body, .. }
                | Stmt::For { body, .. } => {
                    if Self::body_contains_return(body) {
                        return true;
                    }
                }
                Stmt::Loop { init, body, .. } => {
                    if let Some(init) = init
                        && Self::body_contains_return(std::slice::from_ref(init.as_ref()))
                    {
                        return true;
                    }
                    if Self::body_contains_return(body) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    pub(super) fn body_contains_non_nil_return(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            match stmt {
                Stmt::Return(expr) => {
                    if !matches!(expr, Expr::Literal(lit) if lit.is_nil()) {
                        return true;
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if Self::body_contains_non_nil_return(then_branch)
                        || Self::body_contains_non_nil_return(else_branch)
                    {
                        return true;
                    }
                }
                Stmt::While { body, .. }
                | Stmt::React { body }
                | Stmt::SyntheticBlock(body)
                | Stmt::Block(body)
                | Stmt::Subtest { body, .. } => {
                    if Self::body_contains_non_nil_return(body) {
                        return true;
                    }
                }
                Stmt::For { body, .. } => {
                    if Self::body_contains_non_nil_return(body) {
                        return true;
                    }
                }
                Stmt::Loop { init, body, .. } => {
                    if let Some(init) = init
                        && Self::body_contains_non_nil_return(std::slice::from_ref(init.as_ref()))
                    {
                        return true;
                    }
                    if Self::body_contains_non_nil_return(body) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }
}
