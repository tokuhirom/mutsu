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
use crate::value::{RuntimeError, SubData, Value, ValueView};

/// The `X::Multi::NoMatch` Raku's `++`/`--` multi raises for an argument with
/// no container to write: `op` is the routine (`postfix:<++>`, ...), `arg`
/// the value passed (`None` when it is not at hand).
///
/// The message is Rakudo's `X::Multi::NoMatch.message` over the operator's
/// core candidates (`rakudo_candidates.txt`): the candidates whose parameter
/// accepts the argument, held in a fresh container, are listed in declaration
/// order as needing mutable arguments; the rest follow, sorted, as not
/// matching for other reasons.
// Cost: O(c log c), c = core candidates of `op`.
pub(crate) fn incdec_requires_mutable_error(op: &str, arg: Option<&Value>) -> RuntimeError {
    let candidates: Vec<&'static str> =
        crate::builtins::rakudo_candidates::core_sub_candidates(op)
            .iter()
            .map(|c| c.signature.strip_prefix(':').unwrap_or(c.signature))
            .collect();
    let Some(arg) = arg else {
        let mut msg = format!("Cannot resolve caller {op}(...); none of these signatures matches:");
        for cand in &candidates {
            msg.push_str("\n    ");
            msg.push_str(cand);
        }
        return crate::runtime::methods_signature_errors::make_no_match_error_with_message(msg);
    };
    let (rw, mut other): (Vec<&str>, Vec<&str>) = candidates
        .iter()
        .partition(|cand| first_param_accepts(cand, arg));
    let mut msg = format!(
        "Cannot resolve caller {op}({}); the following candidates\n\
         match the type but require mutable arguments:",
        Interpreter::typed_arg_label(arg)
    );
    for cand in &rw {
        msg.push_str("\n    ");
        msg.push_str(cand);
    }
    if !other.is_empty() {
        other.sort_unstable();
        msg.push_str("\n\nThe following do not match for other reasons:");
        for cand in &other {
            msg.push_str("\n    ");
            msg.push_str(cand);
        }
    }
    crate::runtime::methods_signature_errors::make_no_match_error_with_message(msg)
}

/// Whether the first parameter of the signature gist `sig` (`(Int:D $a is rw
/// --> Int:D)`) accepts `arg` by type and definiteness. A native-typed `is rw`
/// parameter accepts any argument here: Rakudo's `Signature.ACCEPTS` of a
/// capture in a fresh `Scalar` does not unbox it, so `(int $a is rw)` is
/// listed as matching even a `Str:D`.
// Cost: O(t), t = length of the parameter's type name.
fn first_param_accepts(sig: &str, arg: &Value) -> bool {
    let Some(ty) = sig.trim_start_matches('(').split_whitespace().next() else {
        return false;
    };
    let (base, smiley) = match ty.rsplit_once(':') {
        Some((base, smiley @ ("D" | "U"))) => (base, Some(smiley)),
        _ => (ty, None),
    };
    if base.starts_with(|c: char| c.is_ascii_lowercase()) {
        return true;
    }
    let (arg_type, defined) = match arg.view() {
        ValueView::Package(name) => (name.to_string(), false),
        _ => (
            crate::runtime::utils::value_type_name(arg).to_string(),
            crate::runtime::types::value_is_defined(arg),
        ),
    };
    let definite_ok = match smiley {
        Some("D") => defined,
        Some("U") => !defined,
        _ => true,
    };
    definite_ok && Interpreter::type_matches(base, &arg_type)
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
                // Raku calls the routine, then finds no container in what
                // it returned.
                let result = self.call_function(&name, call_args)?;
                return self.builtin_incdec_nomatch(&[op_label, result]);
            };
            let old = self
                .call_sub_value(callable.clone(), call_args.clone())?
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
