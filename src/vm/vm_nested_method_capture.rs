//! `OpCode::CaptureNestedMethodEnv`: the lexical capture of a `method`
//! declared in a nested block of a package body.
//!
//! The parser hoists such a declaration to the package body
//! (`parser::stmt::nested_block_methods`), so the package-body walk installs it
//! like any other method -- in the method table, with its return type,
//! private/submethod/multi status and traits intact. What the hoisted copy
//! cannot see is the block it was written in: a `sub helper` or a `my $y`
//! declared next to it. The block keeps a `Stmt::NestedMethodCapture` marker
//! in the declaration's place, which builds an anonymous method closure over
//! the same signature and body; the closure machinery computes the capture of
//! the block's variables, this op adds the block's routines, and files the
//! result for the hoisted method to take.

use crate::opcode::NestedMethodCaptureSpec;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

impl Interpreter {
    // Cost: O(e + f + n * r + m), e = the closure's env entries, f = the
    // body's free variables, n = the enclosing blocks' routines, r = the cost
    // of one `&name` resolution (`resolve_code_var`), m = the package's
    // methods (only when the marker runs in a routine body, not the package
    // body).
    pub(super) fn exec_capture_nested_method_env_op(&mut self, spec: &NestedMethodCaptureSpec) {
        let Some(closure) = self.stack.pop() else {
            return;
        };
        let ValueView::Sub(data) = closure.view() else {
            return;
        };
        let mut env = data.env.clone();
        // Keep only the user lexicals the body (and its signature) actually
        // reads: the closure env also carries the block's unrelated bindings,
        // and the capture is installed authoritatively on every call.
        if let Some(code) = &data.compiled_code {
            let mut reads: rustc_hash::FxHashSet<Symbol> =
                code.free_var_syms.iter().copied().collect();
            let compiler = crate::compiler::Compiler::new();
            reads.extend(compiler.decl_time_param_free_var_syms(&data.param_defs));
            env.retain(|sym, _| {
                reads.contains(sym)
                    && sym.with_str(|name| {
                        crate::env::is_plain_user_lexical(name)
                            || (name.starts_with(['@', '%', '&', '$'])
                                && crate::env::is_user_variable_key(name))
                    })
            });
        }
        // The block's `sub`s and `proto`s are captured too, as the `&name`
        // lexicals they are. They live in the routine registry rather than in
        // the closure env, and block exit restores the registry, so resolve
        // each one now, while the block is live. Both a bare call and a
        // `&name` read in the method body consult the `&name` binding ahead of
        // the registry.
        for routine in &spec.routines {
            let value = routine.with_str(|name| self.resolve_code_var(name));
            if !value.is_nil() {
                env.insert(format!("&{}", routine.resolve()), value);
            }
        }
        // Authoritative, like any declared method's capture: a same-named
        // lexical in the caller must not shadow the block's binding.
        env.insert_sym(
            Symbol::intern("__mutsu_declared_method_capture"),
            Value::int(1),
        );
        let Some(owner) = self.nested_capture_owners.last().copied() else {
            // Not a package-body walk: the marker sits in the body of a
            // routine of the package (`method ^find_method { multi method
            // handler { ... $name ... } }`), and this is one of its calls.
            // The hoisted method is installed already; like rakudo, it closes
            // over the routine's latest invocation, so give it this capture.
            let owner = self.lexical_closure_package_sym();
            let index = spec.index;
            self.registry_mut().map_user_methods_in_place(owner, |def| {
                if def.nested_capture_index == Some(index) && def.role_origin.is_none() {
                    def.captured_env = Some(env.clone());
                }
            });
            return;
        };
        self.nested_method_captures.insert((owner, spec.index), env);
    }

    /// Give the captures a role body's nested blocks just filed (under
    /// `role`, see [`Self::exec_capture_nested_method_env_op`]) to the
    /// methods they belong to: the copies composed into `target_class`, and
    /// the role's own definitions, which a later mixin or copy reads.
    ///
    /// A role body runs again at every composition, so each composition
    /// gets its own capture (`role R[$n] { do { my $q = $n; method m { $q } } }`
    /// answers per parameterization). The captures are also kept for the
    /// composition, so a re-registration of `target_class` that the
    /// composition memo keeps from re-running the body gets them back
    /// ([`Self::reapply_composed_nested_method_captures`]).
    // Cost: O(c + m + k), c = pending captures, m = methods of `target_class`
    // and `role`, k = the role's candidates and their methods.
    pub(crate) fn apply_nested_method_captures(&mut self, role: &str, target_class: &str) {
        let role_sym = Symbol::intern(role);
        let mut captures: rustc_hash::FxHashMap<u32, crate::env::Env> =
            rustc_hash::FxHashMap::default();
        self.nested_method_captures.retain(|(owner, index), env| {
            if *owner == role_sym {
                captures.insert(*index, std::mem::take(env));
                false
            } else {
                true
            }
        });
        if captures.is_empty() {
            return;
        }
        self.give_nested_method_captures(role, target_class, &captures, true);
        self.composed_nested_method_captures
            .insert((Symbol::intern(target_class), role_sym), captures);
    }

    /// Give `target_class`'s methods composed from `role` the captures the
    /// role body filed when this same composition first ran. A class
    /// registered again (the in-place registration of a nested declaration
    /// after its compile-time shell, or a redeclaration on every pass of a
    /// loop) rebuilds its composed methods, but the composition memo
    /// (`Registry::composed_role_bodies`) keeps the role body from running
    /// again, so nothing would file new captures for them.
    // Cost: O(c + m), c = the composition's captures, m = methods of
    // `target_class`.
    pub(crate) fn reapply_composed_nested_method_captures(
        &mut self,
        role: &str,
        target_class: &str,
    ) {
        let key = (Symbol::intern(target_class), Symbol::intern(role));
        let Some(captures) = self.composed_nested_method_captures.get(&key).cloned() else {
            return;
        };
        self.give_nested_method_captures(role, target_class, &captures, false);
    }

    // Cost: O(c + m + k), as `apply_nested_method_captures`; k = 0 unless
    // `to_role` is set.
    fn give_nested_method_captures(
        &mut self,
        role: &str,
        target_class: &str,
        captures: &rustc_hash::FxHashMap<u32, crate::env::Env>,
        to_role: bool,
    ) {
        let declared_by_role = |def: &crate::runtime::MethodDef| {
            def.original_role
                .as_deref()
                .or(def.role_origin.as_deref())
                .is_none_or(|origin| origin == role)
        };
        let mut registry = self.registry_mut();
        registry.map_user_methods_in_place(Symbol::intern(target_class), |def| {
            if let Some(index) = def.nested_capture_index
                && def.role_origin.is_some()
                && declared_by_role(def)
                && let Some(env) = captures.get(&index)
            {
                def.captured_env = Some(env.clone());
            }
        });
        if !to_role {
            return;
        }
        let give_role_def = |role_def: &mut crate::runtime::RoleDef| {
            for defs in role_def.methods.values_mut() {
                for def in defs.iter_mut() {
                    if let Some(index) = def.nested_capture_index
                        && def.role_origin.is_none()
                        && let Some(env) = captures.get(&index)
                    {
                        def.captured_env = Some(env.clone());
                    }
                }
            }
        };
        let Some(role_id) = registry.roles.get_mut(role).map(|role_def| {
            give_role_def(role_def);
            role_def.role_id
        }) else {
            return;
        };
        // A mixin (`1 but R`) reads the role's methods from its
        // `role_candidates` entry (`role_def_for_mixin_role`), a separate copy
        // of the same `RoleDef`, so that copy needs the capture too.
        if let Some(candidates) = registry.role_candidates.get_mut(role) {
            for candidate in candidates.iter_mut() {
                if candidate.role_def.role_id == role_id {
                    give_role_def(&mut candidate.role_def);
                }
            }
        }
    }
}
