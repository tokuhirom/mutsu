//! Plain `@x`/`%x` positional parameters on the light call paths.
//!
//! The general binder (`bind_function_args_values`) binds such a parameter to
//! the caller's container and registers an exit-time writeback of the
//! parameter's final value to the caller's variable. Neither is needed for an
//! untyped `Array`/`Hash` argument: mutsu's containers are shared by
//! reference, and assignment to one (`@a = ...`, `%h = ...`), like `.push` or
//! an element store, mutates it in place, so binding the same container is the
//! whole of Raku's readonly-container semantics. The light paths therefore
//! bind that shape directly, and keep every other one on the general binder.
//! Measured on YAMLish's `concretize($schema, $namespaces, %anchors,
//! %callbacks)` methods, the full binder cost ~40k instructions per `%`
//! parameter per call (#12917).

use super::*;

impl Interpreter {
    /// Whether every plain `@x`/`%x` positional parameter
    /// ([`crate::opcode::FastParamCheck::light_aggregate_sigil`]) receives, at
    /// THIS call, an argument the light bind can take as it is: an
    /// `Array`/`List` for `@`, a `Hash` for `%`. The light bind then
    /// shares that container with the caller, which is all the general binder
    /// achieves with its exit-time writeback, since assignment to a mutsu
    /// `Array`/`Hash` stores in place.
    ///
    /// Everything else declines to the general binder, unchanged: a `Seq`
    /// (bound through `PositionalBindFailover`), a `Pair`/`Map`/`Set` or other
    /// Associative, an object hash, a shaped or lazy array, a type object (an arity/type
    /// error the binder reports), and a missing argument. A container that
    /// carries element/key type metadata (`my Int @a`) is admitted: the light
    /// bind registers its constraint under the parameter's name exactly as
    /// the binder does ([`Self::bind_light_aggregate_constraint`]). Like
    /// [`Self::positional_light_rw_args_admitted`] it is re-derived per call,
    /// because one routine is called with every shape; the flag check keeps a
    /// routine with no such parameter free of it.
    // Cost: O(k), k = plain `@`/`%` positional parameters.
    #[inline]
    pub(super) fn positional_light_aggregate_args_admitted(
        cf: &CompiledFunction,
        args: &[Value],
    ) -> bool {
        // Inlined down to this test, so a routine with no such parameter (the
        // `fib` hot path) pays one branch and no call.
        cf.aggregate_positional_params == 0 || Self::aggregate_args_admitted_masked(cf, args)
    }

    /// The argument walk of [`Self::positional_light_aggregate_args_admitted`].
    // Cost: O(k), k = plain `@`/`%` positional parameters.
    #[inline(never)]
    fn aggregate_args_admitted_masked(cf: &CompiledFunction, args: &[Value]) -> bool {
        let mut mask = cf.aggregate_positional_params;
        while mask != 0 {
            let i = mask.trailing_zeros() as usize;
            mask &= mask - 1;
            let Some(arg) = args.get(i) else {
                return false;
            };
            let sigil = if cf.param_defs[i].name.starts_with('@') {
                '@'
            } else {
                '%'
            };
            if !Self::light_aggregate_arg_admitted(sigil, arg) {
                return false;
            }
        }
        true
    }

    /// One argument of [`Self::positional_light_aggregate_args_admitted`].
    // Cost: O(1).
    pub(super) fn light_aggregate_arg_admitted(sigil: char, arg: &Value) -> bool {
        match (sigil, arg.unwrap_varref().view()) {
            ('@', ValueView::Array(_, kind)) => {
                matches!(
                    kind,
                    ArrayKind::Array | ArrayKind::List | ArrayKind::ItemArray | ArrayKind::ItemList
                )
            }
            // An object hash (`my %h{Mu}`) keeps the binder: its keyed
            // stores are not in place, so the caller only sees them through
            // the binder's writeback (Cro's `%connection-state{$type}`).
            ('%', ValueView::Hash(map)) => map.key_type.is_none(),
            _ => false,
        }
    }

    /// The method-dispatch twin of
    /// [`Self::positional_light_aggregate_args_admitted`]: true when every
    /// positional `@`/`%` parameter that would otherwise force the full binder
    /// (`has_aliasable_container_params` in `call_compiled_method_in_unit`) is
    /// a plain one ([`crate::opcode::FastParamCheck::light_aggregate_sigil`])
    /// receiving an argument the fast binder may share as it is. A method's
    /// `param_defs` include the invocant and its named parameters while `args`
    /// hold neither, so positionals are aligned to the arguments by a counter
    /// that skips named (string-keyed `Pair`) arguments, as the binder does.
    /// True for a method with no such parameter at all.
    // Cost: O(p + a), p = parameters, a = arguments.
    pub(super) fn method_aggregate_args_light(
        method_def: &crate::runtime::MethodDef,
        args: &[Value],
    ) -> bool {
        let mut arg_idx = 0usize;
        for pd in &method_def.param_defs {
            if pd.is_invocant || pd.named || pd.slurpy || pd.double_slurpy {
                continue;
            }
            while args
                .get(arg_idx)
                .is_some_and(|a| a.unwrap_varref().is_string_pair_value())
            {
                arg_idx += 1;
            }
            let this_idx = arg_idx;
            arg_idx += 1;
            if !pd.name.starts_with(['@', '%']) {
                continue;
            }
            let Some(sigil) = crate::opcode::FastParamCheck::light_aggregate_sigil(pd) else {
                // A twigilled (`@!x`) or `is copy` parameter never forced the
                // full binder; every other shape still does.
                if pd.name[1..].starts_with(['!', '.']) || pd.traits.iter().any(|t| t == "copy") {
                    continue;
                }
                return false;
            };
            match args.get(this_idx) {
                Some(arg) if Self::light_aggregate_arg_admitted(sigil, arg) => {}
                _ => return false,
            }
        }
        true
    }

    /// Register the constraint a light-bound plain `@x`/`%x` parameter
    /// carries in the callee's scope, as the general binder does for it: the
    /// argument container's own element (and key) type when it has one
    /// (`my Int %h` passed to `sub f(%p)` keeps rejecting `%p = (a => 'x')`),
    /// and none otherwise, which shadows a same-named typed variable of an
    /// outer scope (`my Int %p` around `sub f(%p)` must not constrain it).
    /// Must run once the callee's env is in place.
    // Cost: O(1) for an untyped container; O(t) for a typed one, t = the
    // length of its type's spelling.
    pub(super) fn bind_light_aggregate_constraint(&mut self, name: &str, sym: Symbol, val: &Value) {
        let typed = self
            .container_type_metadata(val)
            .filter(|info| !info.value_type.is_empty())
            .map(|info| match info.key_type {
                Some(kt) => format!("{}{{{}}}", info.value_type, kt),
                None => info.value_type,
            });
        // No constraint to register, and no `__mutsu_type::` record under this
        // name anywhere to shadow: the binder's own early return for that case,
        // without resolving the name again (#12526).
        if typed.is_none() && !Self::env_type_constraint_seen_for(sym) {
            return;
        }
        self.bind_param_type_constraint_sym(name, sym, typed.as_deref());
    }
}
