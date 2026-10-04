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

//!
//! A name that resolves to no routine but to an `&`-variable holding a code
//! object (`my &k = sub h($x is rw) is rw { $x }; k($v)++`) steps through that
//! code object, the way `k($v) = 5` assigns through it (#10965).

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, SubData, Value, ValueView};

/// The `X::Multi::NoMatch` Raku's `++`/`--` multi raises for an argument with
/// no container to write: `op` is the routine (`postfix:<++>`, ...), `arg`
/// what the message names it by (a variable name, `Int:D`, `...`).
// Cost: O(n), n = length of the message.
pub(crate) fn incdec_requires_mutable_error(op: &str, arg: &str) -> RuntimeError {
    let msg =
        format!("Cannot resolve caller {op}({arg}); the parameter requires mutable arguments");
    let mut err = RuntimeError::new(msg.clone());
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg));
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::Multi::NoMatch"),
        attrs,
    )));
    err
}

impl Interpreter {
    /// Whether a named routine exposes a writable call result: declared `is rw`
    /// / `is raw`, or spelling an explicit `return-rw` (which is assignable on
    /// its own) — see `routine_is_rw_capable`.
    fn named_sub_is_rw_capable(&mut self, name: &str, call_args: &[Value]) -> bool {
        self.resolve_function_with_alias(name, call_args)
            .is_ok_and(|def| def.is_some_and(|def| Self::routine_is_rw_capable(&def)))
    }

    /// [`Interpreter::routine_is_rw_capable`] asked of a routine code object
    /// rather than its `FunctionDef`. A body-less code object (ADR-0019
    /// C6e-3b) answers the `return-rw` question through its
    /// `compiled_routine`, so it never has to be re-resolved by its declared
    /// name — which may be lexical to another unit (an EVAL) or be the very
    /// `&`-variable the call went through (#10965).
    // Cost: O(1) for a declared rw / raw routine or a compiled routine;
    // otherwise O(b), b = body AST nodes (the `return-rw` scan).
    pub(crate) fn sub_is_rw_capable(data: &SubData) -> bool {
        data.is_rw
            || data.is_raw
            || data
                .compiled_routine
                .as_ref()
                .is_some_and(|cf| cf.returns_container())
            || crate::opcode::body_uses_return_rw(&data.body)
    }

    /// The `&name` code object a call to `name` dispatches through when no
    /// routine of that name resolves, if it is a rw-capable routine.
    fn rw_capable_callable_var(&self, name: &str) -> Option<Value> {
        let callable = self.env.get(&format!("&{name}"))?;
        let rw = match callable.view() {
            ValueView::Sub(data) => Self::sub_is_rw_capable(&data),
            ValueView::WeakSub(weak) => weak
                .upgrade()
                .is_some_and(|strong| Self::sub_is_rw_capable(&strong)),
            _ => false,
        };
        rw.then(|| callable.clone())
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
            let Some(callable) = self.rw_capable_callable_var(&name) else {
                return self.builtin_incdec_nomatch(std::slice::from_ref(&op_label));
            };
            let old = self
                .call_sub_value(callable.clone(), call_args.clone(), true)?
                .deref_container();
            let new = if is_inc {
                self.increment_value_smart(&old)?
            } else {
                self.decrement_value_smart(&old)?
            };
            self.assign_callable_lvalue_with_values(callable, call_args, new.clone())?;
            return Ok(if is_prefix { new } else { old });
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
