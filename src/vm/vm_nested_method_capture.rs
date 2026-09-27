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
    // Cost: O(e + f + n * r), e = the closure's env entries, f = the body's
    // free variables, n = the enclosing blocks' routines, r = the cost of one
    // `&name` resolution (`resolve_code_var`).
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
        let owner = self
            .nested_capture_owners
            .last()
            .copied()
            .unwrap_or_else(|| self.current_package_sym());
        self.nested_method_captures.insert((owner, spec.index), env);
    }

    /// Give the captures a role body's nested blocks just filed (under
    /// `role`, see [`Self::exec_capture_nested_method_env_op`]) to the
    /// methods they belong to: the copies composed into `target_class`, and
    /// the role's own definitions, which a later mixin or copy reads.
    ///
    /// A role body runs again at every composition, so each composition
    /// gets its own capture (`role R[$n] { do { my $q = $n; method m { $q } } }`
    /// answers per parameterization).
    // Cost: O(c + m), c = pending captures, m = methods of `target_class` and `role`.
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
        if let Some(role_def) = registry.roles.get_mut(role) {
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
        }
    }
}
