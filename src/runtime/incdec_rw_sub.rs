//! `++`/`--` applied to the result of a call to an `rw` routine.
//!
//! Raku's `prefix:<++>` takes its argument `is rw`, so `++f()` is only legal
//! when `f` hands back a container — an `is rw` routine, or one whose tail is an
//! explicit `return-rw`. mutsu compiles every such form (`++f()`, `--f()`,
//! `f()++`, `f()--`) to a call of `__mutsu_incdec_named_sub_lvalue`, which
//! decides at RUNTIME whether the named routine is rw-capable: the compiler
//! cannot know, because the routine may be declared after the use site.
//!
//! When it is not rw-capable we raise the very same `X::Multi::NoMatch` that a
//! bare `++42` raises ("the parameter requires mutable arguments"), so the
//! diagnostic for `++non_rw_sub()` is unchanged.
//!
//! When it is, we read the current value by calling the routine, apply the
//! `.succ`/`.pred` step, and write the result back through the existing rw-sub
//! lvalue assignment path (`assign_named_sub_lvalue_with_values`) — the same
//! mechanism `f() = $v` uses. This calls the routine twice (once to read, once
//! to resolve the write target), matching what mutsu already does for the
//! method-accessor form `$obj.attr++`.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

impl Interpreter {
    /// Whether a named routine exposes a writable call result: declared `is rw`
    /// / `is raw`, or spelling an explicit `return-rw` (which is assignable on
    /// its own) — see `routine_is_rw_capable`.
    fn named_sub_is_rw_capable(&mut self, name: &str, call_args: &[Value]) -> bool {
        self.resolve_function_with_alias(name, call_args)
            .is_some_and(|def| Self::routine_is_rw_capable(&def))
    }

    /// `__mutsu_incdec_named_sub_lvalue(name, [args], op_label)`
    ///
    /// `op_label` is one of `prefix:<++>` / `prefix:<-->` / `postfix:<++>` /
    /// `postfix:<-->`, so it carries both the direction and the position as well
    /// as being the text of the fallback `X::Multi::NoMatch` message.
    pub(crate) fn builtin_incdec_named_sub_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_incdec_named_sub_lvalue expects name, call args, and op label",
            ));
        }
        let name = args[0].to_string_value();
        let call_args = Self::sub_call_args_from_value(args.get(1));
        let op_label = args[2].clone();
        let label = op_label.to_string_value();
        let is_inc = label.contains("++");
        let is_prefix = label.starts_with("prefix");

        if !self.named_sub_is_rw_capable(&name, &call_args) {
            return self.builtin_incdec_nomatch(std::slice::from_ref(&op_label));
        }

        // An rw routine whose tail is `return-rw @a[$i]` hands back the
        // element's shared container, not its value (that is the whole point of
        // `compile_return_rw_arg`). Read through it before stepping, or the
        // `.succ` would be applied to the container itself.
        let old = self
            .call_function(&name, call_args.clone())?
            .deref_container();
        let new = if is_inc {
            self.increment_value_smart(&old)?
        } else {
            self.decrement_value_smart(&old)?
        };
        self.assign_named_sub_lvalue_with_values(&name, call_args, new.clone())?;
        Ok(if is_prefix { new } else { old })
    }
}
