//! The by-name resolution chain of a `GetGlobal` read that the fast scalar
//! shortcut in `vm_get_global.rs` did not serve.

use super::*;

impl Interpreter {
    /// The value the stores answer for `name`, in the chain's precedence order;
    /// `loop_param_val` is the running `for` parameter's binding when the name
    /// is one. The caller applies the `OUR::`, default and lazy-thunk steps.
    // Cost: O(d + p*|name|), d = env tiers walked, p = enclosing packages probed.
    pub(super) fn get_global_chain_value(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        name: &str,
        loop_param_val: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(val) = loop_param_val {
            Ok(val)
        } else {
            self
                // A PER-CALL anonymous state (`$` inside a block inside a
                // routine) is authoritative in the state store: its `env`
                // entry, written by `SetGlobal`, outlives the block clone
                // and is always the previous call's. See `anon_state_key`.
                .per_call_anon_state_read(name, Value::NIL)
                // A package-block `my` lexical is stored in `package_lexicals`;
                // it is the authoritative store for a bare free-variable read from
                // inside that package's named subs, and must be read BEFORE `env`.
                // Two stale `env` shadows would otherwise win: a boxed lexical's
                // prior-call return-merge copy, and the package block's own
                // `my $x` top-level local slot flushed to `env` as the type object
                // after the block exits (`sync_env_from_locals`). Gated on a real
                // `current_package`, so a bare reference after the block (under
                // GLOBAL) does not resolve here.
                // An `our sub` declared in a bare block reads a block `my`
                // lexical that is out of scope by the time the registry routine
                // runs (no per-sub closure env), and the shared `env` may hold an
                // unrelated leaked value from a sibling block. Resolve such a
                // capture through its persisted shared cell ONLY — see
                // `escaping_our_read` — short-circuiting the env lookup.
                .or_else(|| self.escaping_our_read(name))
                .or_else(|| self.eval_unit_package_capture(name))
                .or_else(|| self.package_scope_lexical_in(code, name))
                // A package-qualified variable lives in `our_vars`; an env
                // entry under its name is a copy some frame left behind
                // (a by-name write stores both), which a later write from
                // another frame does not update. The return merges no
                // longer carry such a copy back (ADR-12529 §3, slice 38),
                // so read the package store first.
                .or_else(|| {
                    if crate::qualified::is_qualified(code.const_sym(name_idx)) {
                        self.get_our_var(name)
                    } else {
                        None
                    }
                })
                // NB: `get_env_with_main_alias` is also where a file-scope `my`
                // of the running routine's own compunit resolves — `env` is not
                // authoritative for that name. See `unit_lexicals`.
                // The name is a constant-pool entry, so hand over its
                // memoized `Symbol` rather than making the chokepoint
                // re-intern it on every read. This is the only read path an
                // `@`/`%` name has -- the scalar shortcut above excludes
                // those sigils -- so a container-heavy program paid one
                // intern per element access without it.
                .or_else(|| self.get_env_with_main_alias_sym(name, code.const_sym(name_idx)))
                .or_else(|| {
                    // Fall back to the persistent our_vars store for `our`-scoped
                    // variables accessed via package-qualified names (e.g., $Pkg::var).
                    // Bare variable names should NOT fall back to our_vars — the
                    // lexical alias for `our` variables is block-scoped.
                    if crate::qualified::is_qualified(code.const_sym(name_idx)) {
                        self.get_our_var(name)
                            .or_else(|| self.our_var_pseudo_unqualified(name))
                            .or_else(|| {
                                // Nested package shorthand: when looking up
                                // `$D2::d3` from inside package `D1::D2` (or any
                                // ancestor of it), also try the fully-qualified
                                // forms by prepending each ancestor prefix.
                                // The cheap gate first: the common case is
                                // GLOBAL.
                                if self.current_package_is_global() {
                                    return None;
                                }
                                // Walk up the current package, longest prefix
                                // first, trying each joined with the requested
                                // name (sigil kept in front). The walk and each
                                // `<sigil><prefix>::<name>` key are memoized per
                                // symbol; the empty prefix would rebuild the
                                // name itself, which already missed above.
                                let name_sym = code.const_sym(name_idx);
                                for pkg in
                                    crate::qualified::package_ancestors(self.current_package_sym())
                                {
                                    let candidate = crate::qualified::qualified_var(pkg, name_sym);
                                    if let Some(v) = self.get_our_var(candidate.as_str()) {
                                        return Some(v);
                                    }
                                    if let Some(v) =
                                        self.get_env_with_main_alias(candidate.as_str())
                                    {
                                        return Some(v);
                                    }
                                }
                                None
                            })
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    // Outer-lexical fallback: when a package-qualified name
                    // (e.g. `A::x`) is not found in any package store, fall
                    // back to looking up just the bare component (`x`) in env.
                    // This handles class body statements that access outer
                    // lexical variables which are stored in env under their
                    // unqualified names (not as `A::x`).
                    // The split is memoized per symbol: no `"::"` search
                    // on each miss.
                    let split = crate::qualified::split_qualified_var(code.const_sym(name_idx))?;
                    // Only apply when the qualifier matches the current package
                    // (i.e. the name was auto-qualified by the compiler, not
                    // explicitly written as a package-qualified access).
                    if self.current_package_is_global() {
                        return None;
                    }
                    // The bare component after the last `::`.
                    if split.bare.is_empty() {
                        return None;
                    }
                    // A qualifier naming another package is an explicit
                    // package-variable access: `$UserTimezone::timezone`
                    // read inside `EXPORT::override` must not answer the
                    // routine's own `$timezone` parameter (User::Timezone).
                    // The compiler qualifies such a name with the package
                    // it is written in or one enclosing it (`A::x` in a
                    // class nested in `A`); any other qualifier was
                    // written explicitly.
                    let current = self.current_package_sym();
                    if !crate::qualified::package_ancestors(current).any(|pkg| {
                        pkg.as_str() == split.pkg
                            || crate::qualified::last_segment(pkg).as_str() == split.pkg
                    }) {
                        return None;
                    }
                    self.get_env_with_main_alias(split.bare)
                })
                // Bare-name package-chain fallback: `our` variables and
                // package-block `my` lexicals of the enclosing package
                // chain (a named sub's `current_package` is its package; a
                // method's is its owner class, whose qualified name walks
                // up to the enclosing module). See
                // `package_chain_var_fallback`.
                .or_else(|| self.package_chain_var_fallback(code.const_sym(name_idx)))
                // `$E::v` through `constant E = A::B` (#11315).
                .or_else(|| self.package_alias_var_read(name))
                // Anonymous state variable (`$`): fall back to persisted
                // state so the value survives across closure calls.
                .or_else(|| self.anon_state_value(name))
                // `$0`/`$1`/... are `$/[0]`/`$/[1]`/...  A successful match
                // exports each positional capture as its own digit env key,
                // but a directly bound/assigned `$/` (`my $/ := "foobar"`)
                // has none — derive the value by indexing the current `$/`,
                // matching Raku's `$0 == $/[0]` for any object (a non-Match
                // scalar self-indexes: `.[0]` is the value, `.[N>0]` is Nil).
                .or_else(|| {
                    if name.is_empty() || !name.bytes().all(|b| b.is_ascii_digit()) {
                        return None;
                    }
                    let slash = self.get_env_with_main_alias("/")?.deref_container();
                    if slash.is_nil() {
                        return None;
                    }
                    let i: usize = name.parse().ok()?;
                    Some(Self::bound_slash_positional(&slash, i))
                })
                // A file-scope lexical of the module the running routine
                // belongs to. A module body executes in the env of whatever
                // frame loaded it, so that binding dies with the frame (a
                // `require` inside a method); `module_scope_lexicals` keeps it
                // attached to the module. Last resort, after every live store.
                .or_else(|| self.module_scope_lexical(name))
                // A `PROCESS::<$name> := ...` install that ran in a frame
                // which has since exited: `env`'s parent chain no longer
                // has it, but `process_dynamics` outlives every frame
                // (#8682). Also last resort — a live `env`/dynamic-scope
                // binding for the same name always wins.
                .or_else(|| self.get_process_dynamic(name))
                .map(Ok)
                .unwrap_or_else(|| {
                    if name.starts_with('^') {
                        Ok(Value::TRUE)
                    } else if name == "self" || name.ends_with("::self") {
                        Err(RuntimeError::new(
                            "'self' used where no object is available".to_string(),
                        ))
                    } else if name.starts_with('!')
                        && name.len() > 1
                        && name[1..]
                            .chars()
                            .next()
                            .is_some_and(|c| c.is_alphanumeric() || c == '_')
                    {
                        if self.get_env_self().is_some() {
                            Ok(Value::NIL)
                        } else {
                            Err(RuntimeError::new(format!(
                                "Variable $!{} used where no 'self' is available",
                                &name[1..]
                            )))
                        }
                    } else if name == "_" {
                        // An UNSET topic (no `$_` entry in any scope) reads
                        // as Any, not Nil (`$_ === Any` at the top level,
                        // S02-types/nil.t 39). Only the not-found fallback:
                        // a topic explicitly set to Nil (e.g. `Xorelse`
                        // topicalizing a Nil operand) must stay Nil.
                        Ok(Value::package(crate::symbol::wk::any()))
                    } else if name.starts_with("__ANON_STATE#") {
                        // An anonymous scalar (`$`) is a declared but
                        // uninitialized scalar: it reads as the Any type
                        // object, like `my $x` (S03-operators/context.t).
                        Ok(Value::package(crate::symbol::wk::any()))
                    } else if name.starts_with('*')
                        && !runtime::utils::is_builtin_dynamic_var(name)
                        && !code.dynamic_toplevel_declared_sym.contains(
                            &crate::symbol::Symbol::intern(
                                name.trim_start_matches(['$', '@', '%', '&']),
                            ),
                        )
                    {
                        // A `$*x`/`@*x`/`%*x` that resolved through NONE of
                        // the stores above (env, a caller's `my $*x`,
                        // `PROCESS::`, ...) was never declared anywhere in
                        // the dynamic scope: raku raises "Dynamic variable
                        // ... not found" (X::Dynamic::NotFound). It is a
                        // lazy Failure, not an eager throw here — `.^name`
                        // and `.defined` must answer without exploding
                        // (mirrors `CheckDynamicVarDeclared`'s eager throw
                        // on the ASSIGNMENT side, where there is no lazy
                        // value to hand back instead).
                        //
                        // `dynamic_declared_sym` (populated as `my $*x`
                        // compiles anywhere in THIS chunk's body) excludes
                        // a name this exact chunk declares later: `say
                        // $*a; my $*a;` must still read as Nil here so
                        // execution reaches the `my $*a` declaration's own
                        // compiled X::Dynamic::Postdeclaration throw
                        // (roast/S02-names-vars/contextual.t) instead of
                        // exploding on the read first. Chunk-wide rather
                        // than lexical-block-scoped -- a single-pass
                        // compiler cannot know at an earlier read's
                        // compile time whether a later same-block
                        // declaration is coming.
                        let display = if name.starts_with(['@', '%', '&']) {
                            name.to_string()
                        } else {
                            format!("${}", name)
                        };
                        Ok(self.fail_error_to_failure_value(
                            &runtime::utils::dynamic_not_found_error(&display),
                        ))
                    } else if self.module.strict_mode && !Self::strict_read_exempt(name) {
                        // Read-side counterpart of the `SetGlobal` write
                        // check above: a plain scalar name that resolved
                        // through NONE of the real stores tried above
                        // (env, unit/package/module lexicals, `our`-vars,
                        // per-call state, ...) is genuinely undeclared —
                        // `use strict` must reject it instead of silently
                        // yielding Nil (`my $x = $y;` under `use strict`).
                        // `strict_read_exempt` carves out the pseudo-
                        // variable / dynamic-scope shapes `GetGlobal`
                        // also carries that are not ordinary lexicals.
                        Err(self.strict_undeclared_error(name))
                    } else if !name.is_empty()
                    && !name.starts_with(['@', '%', '&', '*', '?', '!'])
                    && name != "/"
                    // The match cursor `$¢` is `Nil` outside a match,
                    // like `$/` (rakudo).
                    && name != "\u{a2}"
                    && !name.bytes().all(|b| b.is_ascii_digit())
                    {
                        // A genuinely undeclared scalar reaches here either
                        // as a bare name under `no strict`, or as a
                        // package-qualified name (`$Foo::bar`) that
                        // `strict_read_exempt` always lets through
                        // regardless of `strict_mode`. raku auto-vivifies
                        // both as the untyped `Any` object, not `Nil`
                        // (mirrors the unset-topic default a few lines
                        // above, S02-types/nil.t 39). `$/`, `$!`, and the
                        // digit capture vars keep their own documented
                        // `Nil` default (S02-types/nil.t 109-113) and fall
                        // through to the branch below instead.
                        Ok(Value::package(crate::symbol::wk::any()))
                    } else {
                        Ok(Value::NIL)
                    }
                })
        }
    }
}
