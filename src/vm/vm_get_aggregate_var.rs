//! `GetArrayVar` / `GetHashVar`: an `@` or `%` variable read by name.
//!
//! The opcodes' arms, moved out of `exec_ops_group_01` so the slot form of a
//! routine-nested sub's free-variable read (`GetLexFree`, `vm_lexsub_free_slot.rs`)
//! runs exactly this resolution when its slot holds no binding yet.

use super::*;
use crate::meta_ns::MetaNs;
use crate::value::ValueMap;

impl Interpreter {
    /// Push the `@` variable the constant `name_idx` names, resolved by name.
    // Cost: O(d) env probe; O(e) when the name holds a Hash (a pair list is built),
    // e = entries.
    pub(super) fn exec_get_array_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        // Reject @!attr (private attribute twigil) when no self is available
        if let Some(bare) = name.strip_prefix("@!")
            && !bare.is_empty()
            && bare.as_bytes()[0].is_ascii_alphabetic()
            && self.get_env_self().is_none()
        {
            return Err(RuntimeError::new(format!(
                "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                name
            )));
        }
        // Phase 3 Stage 2b: array attributes (`@!a`/`@.a`) read straight
        // from `self`'s shared cell so a mutation in a nested method frame
        // is visible here.
        if let Some(cell_val) = self.read_self_attr_cell(name) {
            let val = match cell_val.view() {
                ValueView::Hash(map) => Value::real_array(
                    map.iter()
                        .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                        .collect(),
                ),
                _ => cell_val,
            };
            self.stack.push(val);
            *ip += 1;
            return Ok(());
        }
        // A `:=`-bound gather may read the array currently being
        // produced. MakeGather tags that exact self-reference; expose
        // the live take collector instead of recursively forcing the
        // LazyList (or reading its pre-declaration env snapshot).
        if self
            .env()
            .get_sym(MetaNs::GatherSelfRef.key_for_str(name))
            .is_some()
        {
            self.stack
                .push(Value::real_array(self.current_gather_items()));
            *ip += 1;
            return Ok(());
        }
        // A package-qualified array's canonical store is `our_vars`, as
        // for a hash (`GetHashVar`) and a scalar (`GetGlobal`): a rebind
        // in another frame or on another thread (`Q::<@a> := [...]`)
        // lands there, and this frame's env may hold a stale copy.
        let package_val = if crate::qualified::is_package_array(code.const_sym(name_idx)) {
            self.qualified_our_var_read(code.const_sym(name_idx))
                .or_else(|| self.our_var_pseudo_unqualified(name))
        } else {
            None
        };
        let val = package_val
            // A package-block `my @a` read by the package's own subs,
            // ahead of a same-named enclosing lexical (as `GetGlobal`).
            .or_else(|| self.eval_unit_package_capture(name))
            .or_else(|| self.package_scope_lexical_in(code, name))
            // Constant-pool name: hand over the chunk's memoized
            // `Symbol` instead of re-interning it on every array read.
            .or_else(|| self.get_env_with_main_alias_sym(name, code.const_sym(name_idx)))
            .or_else(|| self.global_package_dynamic_fallback(name))
            .or_else(|| self.get_local_by_bare_name(code, name))
            .or_else(|| {
                // Fallback: check bare name in env (for closures capturing params)
                name.strip_prefix('@')
                    .and_then(|bare| self.env().get(bare).cloned())
            })
            .or_else(|| {
                // Fallback for fast-path method dispatch (skip_env_setup=true):
                // @.attr and @!attr are not set in env, so read directly from
                // self's instance attributes when available.
                let attr_name = name
                    .strip_prefix("@.")
                    .or_else(|| name.strip_prefix("@!"))?;
                if attr_name.is_empty() {
                    return None;
                }
                let self_val = self.get_env_self()?;
                if let ValueView::Instance { attributes, .. } = self_val.view() {
                    attributes.as_map().get(attr_name).cloned()
                } else {
                    None
                }
            })
            // A file-scope `my @a` of the running routine's own module
            // (see `module_scope_lexicals`; keys keep the `@` sigil).
            .or_else(|| self.module_scope_lexical(name))
            // A nested class method can close over a class-body array
            // declared by an enclosing package (`class Outer { my @a;
            // class Inner { method m { @a } } }`). Walk package
            // ancestors just as scalar reads do; the immediate package
            // lookup below cannot see Outer from Outer::Inner.
            .or_else(|| self.package_chain_var_fallback(code.const_sym(name_idx)))
            // `$E::v` through `constant E = A::B` (#11315).
            .or_else(|| self.package_alias_var_read(name))
            // Class-body outer-lexical fallback — see the GetHashVar twin.
            .or_else(|| self.auto_qualified_bare_env_read(code.const_sym(name_idx)))
            // Last resort before the undeclared-variable default: a
            // package-block/class-body `my @a` static persisted in
            // `package_lexicals` (persist_class_body_statics /
            // exec_package_scope_op) after its bare `env` key was
            // deliberately removed — the class-body case has no live
            // `env` binding left to find above, so this is the only
            // remaining source of the initialized value. Kept LAST
            // (not ahead of `env`, unlike the scalar `GetGlobal`
            // ordering) because a package-block array/hash that is
            // still `env`-live (e.g. mutated in place via `.push`
            // across calls) must keep resolving through that live
            // binding — package_lexicals only holds this name's
            // declaration-time snapshot, taken once at block exit
            // (#8869).
            .or_else(|| self.eval_unit_package_capture(name))
            .or_else(|| self.package_scope_lexical_in(code, name))
            .map(Ok)
            .unwrap_or_else(|| {
                // A never-declared `@*`-twigil (dynamic) variable: the
                // caller-chain lookup genuinely found nothing, so raku
                // raises `X::Dynamic::NotFound` -- as a lazy Failure
                // here (not an eager throw), mirroring the scalar
                // `GetGlobal` fallback above so `.^name`/`.defined`
                // still answer and `//` still falls through to its RHS.
                // A builtin dynamic (`@*ARGS`, ...) is always
                // "declared" by the setting, so it is exempted the
                // same way the scalar fallback and the assignment-side
                // `CheckDynamicVarDeclared` check are -- and so is a
                // name this exact chunk `my @*x`-declares later on
                // (`dynamic_declared_sym`), deferring to that
                // declaration's own X::Dynamic::Postdeclaration throw
                // instead of exploding on the earlier read (see the
                // scalar `GetGlobal` fallback's fuller comment).
                if name.strip_prefix('@').is_some_and(|n| n.starts_with('*'))
                    && !runtime::utils::is_builtin_dynamic_var(name)
                    && !code
                        .dynamic_declared_sym
                        .contains(&crate::symbol::Symbol::intern(
                            name.trim_start_matches(['$', '@', '%', '&']),
                        ))
                {
                    Ok(self.fail_error_to_failure_value(
                        &runtime::utils::dynamic_not_found_error(name),
                    ))
                } else if crate::qualified::is_package_array(code.const_sym(name_idx)) {
                    // A package-qualified slot nobody has written is
                    // an empty Scalar in raku (`@GLOBAL::nv.raku` is
                    // `Any`); a write stores an itemized Array there
                    // (#10962).
                    Ok(Value::package(crate::symbol::wk::any()))
                } else {
                    // An undeclared plain `@`-sigil variable defaults to
                    // an empty Array (raku auto-declares it as Array
                    // under `no strict`): `@x[2]` is `(Any)`, `@x.end`
                    // is `-1`, `@x.raku` is `[]`. Anonymous `@`-sigil
                    // variables share this default.
                    Ok(Value::real_array(vec![]))
                }
            })?;
        // A whole-container `:=` bind (`my @b := @a`) stores a shared
        // `ContainerRef` cell in the slot so both aliases observe
        // mutations. Decontainerize the top-level cell here so the read
        // yields the inner Array/Hash (the cell is a binding alias, not
        // an array element). Element-level cells are handled at Index.
        let val = val.into_deref();
        // When @-sigil dereferences a Hash, convert to a list of pairs
        let val = match val.view() {
            // An `@`-sigil read strips the Scalar container: `@$x` on an
            // itemized `$x = [1,2,3]` yields the plain Array (flattens /
            // iterates element-wise).
            ValueView::Array(items, kind)
                if kind.is_itemized()
                    && !crate::qualified::is_package_array(code.const_sym(name_idx)) =>
            {
                Value::array_with_kind(items.clone(), kind.decontainerize())
            }
            ValueView::Hash(map) => {
                let pairs: Vec<Value> = map
                    .iter()
                    .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                    .collect();
                Value::real_array(pairs)
            }
            // NOTE: a Buf/Blob is deliberately NOT unwrapped to its
            // element list here. `@$blob` lowers to `$blob.list` (see
            // `@$x` in the parser), so it never reaches this opcode;
            // what does reach it is a genuine `@`-sigil variable whose
            // container IS a Buf (`my @a is Buf`), and there `@a` must
            // stay the Buf itself so `@a ~~ Buf` holds (S02-types/is-type.t).
            // Array-contextualizing a Seq (`@$s`) caches it, so it may be
            // read repeatedly. If the Seq's iterator was already taken
            // (e.g. by `.skip`/`.iterator`) and not cached, throw.
            ValueView::Seq(items) => {
                if items.is_consumed() && !items.is_cached() {
                    return Err(crate::value::seq_consumed_error());
                }
                items.mark_cache_requested();
                val
            }
            _ => val,
        };
        self.stack.push(val);
        *ip += 1;
        Ok(())
    }

    /// Push the `%` variable the constant `name_idx` names, resolved by name.
    // Cost: O(d) env probe, or O(p + r) for `%?RESOURCES` where p is
    // source-path depth and r is the distribution's resource count.
    pub(super) fn exec_get_hash_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        // Reject %!attr (private attribute twigil) when no self is available
        if let Some(bare) = name.strip_prefix("%!")
            && !bare.is_empty()
            && bare.as_bytes()[0].is_ascii_alphabetic()
            && self.get_env_self().is_none()
        {
            return Err(RuntimeError::new(format!(
                "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                name
            )));
        }
        // %?RESOURCES — build from the current package's distribution context
        if name == "%?RESOURCES" {
            let resources = self
                .build_resources_for_source_file(code.source_file)
                .unwrap_or_else(|| self.build_resources_for_package());
            self.stack.push(resources);
            *ip += 1;
            return Ok(());
        }
        // Phase 3 Stage 2b: hash attributes (`%!h`/`%.h`) read straight
        // from `self`'s shared cell (cross-frame visibility).
        if let Some(cell_val) = self.read_self_attr_cell(name) {
            self.stack.push(cell_val);
            *ip += 1;
            return Ok(());
        }
        let name_sym = code.const_sym(name_idx);
        // A package-qualified hash's canonical store is `our_vars`: an
        // element write in another frame persisted it there, and this
        // frame's env may hold a stale copy or none (#10901).
        let package_val = if crate::qualified::is_package_hash(name_sym) {
            self.get_our_var(name)
                .or_else(|| self.our_var_pseudo_unqualified(name))
        } else {
            None
        };
        let val = package_val
            // A package-block `my %h` read by the package's own subs,
            // ahead of a same-named enclosing lexical (as `GetGlobal`).
            .or_else(|| self.eval_unit_package_capture(name))
            .or_else(|| self.package_scope_lexical_in(code, name))
            // Constant-pool name: hand over the chunk's memoized
            // `Symbol` instead of re-interning it on every hash read.
            .or_else(|| self.get_env_with_main_alias_sym(name, name_sym))
            .or_else(|| self.global_package_dynamic_fallback(name))
            .or_else(|| self.get_local_by_bare_name(code, name))
            .or_else(|| {
                name.strip_prefix('%')
                    .and_then(|bare| self.env().get(bare).cloned())
            })
            .or_else(|| {
                // Fallback for fast-path method dispatch (skip_env_setup=true):
                // %.attr and %!attr are not set in env, so read directly from
                // self's instance attributes when available.
                let attr_name = name
                    .strip_prefix("%.")
                    .or_else(|| name.strip_prefix("%!"))?;
                if attr_name.is_empty() {
                    return None;
                }
                let self_val = self.get_env_self()?;
                if let ValueView::Instance { attributes, .. } = self_val.view() {
                    attributes.as_map().get(attr_name).cloned()
                } else {
                    None
                }
            })
            // A file-scope `my %h` of the module the running routine
            // belongs to (see `module_scope_lexicals`; the table keys
            // keep the `%` sigil). Last resort, after every live store —
            // mirrors the scalar fallback in `GetGlobal`.
            .or_else(|| self.module_scope_lexical(name))
            // Nested classes inherit enclosing package-body container
            // lexicals; mirror the array read's package-chain fallback.
            .or_else(|| self.package_chain_var_fallback(code.const_sym(name_idx)))
            // `$E::v` through `constant E = A::B` (#11315).
            .or_else(|| self.package_alias_var_read(name))
            // Outer-lexical fallback, mirroring GetGlobal: each class-body
            // statement compiles as its own chunk, so a read of a `my`
            // declared by an earlier body statement was auto-qualified
            // (`%C::predef`) while the declaration flushed to env under the
            // bare sigiled name (`%predef`). Strip the qualifier and retry
            // when it names the current package.
            .or_else(|| self.auto_qualified_bare_env_read(code.const_sym(name_idx)))
            // Last resort: a package-block/class-body `my %h` static
            // persisted in `package_lexicals` after its bare `env` key
            // was deliberately removed — see the `GetArrayVar` twin's
            // comment (#8869). Kept LAST so a package-block hash that
            // is still `env`-live (mutated in place across calls)
            // keeps resolving through that live binding instead of
            // this store's one-time declaration-time snapshot.
            .or_else(|| self.package_scope_lexical(name));
        match val {
            // Decontainerize a top-level `ContainerRef` cell from a
            // whole-container `:=` bind (`my %h2 := %h`); the read
            // yields the inner Hash.
            Some(v) => self.stack.push(v.into_deref()),
            None => {
                // %ENV (without * twigil) is not declared in Raku;
                // only %*ENV is valid. Throw an undeclared error for %ENV specifically.
                if name == "%ENV" {
                    return Err(RuntimeError::undeclared("name", "%ENV"));
                }
                // A never-declared `%*`-twigil (dynamic) variable
                // mirrors the `@*` case in `GetArrayVar`: raku raises
                // `X::Dynamic::NotFound` there, as a lazy Failure (not
                // an eager throw) so `.^name`/`.defined` still answer
                // and `//` still falls through to its RHS. A builtin
                // dynamic (`%*ENV`, ...) is always "declared" by the
                // setting, exempted the same way the scalar/array
                // fallbacks are -- and so is a name this exact chunk
                // `my %*x`-declares later on (see the scalar
                // `GetGlobal` fallback's fuller comment).
                if name.strip_prefix('%').is_some_and(|n| n.starts_with('*'))
                    && !runtime::utils::is_builtin_dynamic_var(name)
                    && !code
                        .dynamic_declared_sym
                        .contains(&crate::symbol::Symbol::intern(
                            name.trim_start_matches(['$', '@', '%', '&']),
                        ))
                {
                    self.stack.push(self.fail_error_to_failure_value(
                        &runtime::utils::dynamic_not_found_error(name),
                    ));
                } else if crate::qualified::is_package_hash(name_sym) {
                    // A never-written package-qualified slot: see the
                    // `GetArrayVar` twin (#10962).
                    self.stack.push(Value::package(crate::symbol::wk::any()));
                } else {
                    // An undeclared plain `%`-sigil variable defaults to
                    // an empty Hash (raku auto-declares it as Hash under
                    // `no strict`): `%h<k>` is `(Any)`, `%h.raku` is
                    // `{}`. Anonymous `%`-sigil variables share this
                    // default.
                    self.stack.push(Value::hash(ValueMap::default()));
                }
            }
        }
        *ip += 1;
        Ok(())
    }
}
