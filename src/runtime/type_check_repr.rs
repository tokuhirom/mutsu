//! The `(repr)` tail of a type-check message, for a value whose repr needs the
//! interpreter.
//!
//! Rakudo words a failed assignment as `expected R but got F (F.new)`: the
//! offending value's type name, then its `.raku` in parentheses. For a number,
//! a string, a type object or a plain collection (`List`, `Array`, `Hash`,
//! `Pair`, `Range`, `Set`, ...) that text is a pure function of the value
//! (`runtime::utils::value_short_repr`, which renders through `raku_value`).
//! For an *object* it is a method call -- a class may declare its own `raku`,
//! and the default renders the public attributes -- so it cannot be built by
//! the interpreter-free error constructors. This is the interpreter-aware half:
//! it asks the object, then hands the finished text to the pure builder.
//!
//! The user's `raku` runs through compiled method dispatch
//! (`try_dispatch_compiled_method_direct`), and the built-in default through
//! `default_instance_repr`, the same renderer `.raku` itself reaches -- neither
//! is a tree-walk fallback. A container *holding* an object (`[Foo.new]`) is
//! rendered by `raku_element_repr`, the leaf dispatch `.raku` on that container
//! already uses, so the message agrees with `.raku` by construction.

use super::*;
use crate::builtins::methods_0arg::raku_repr::{needs_raku_dispatch, raku_value};
use crate::runtime::container_needs_raku_dispatch;

/// The interned `raku` method name.
fn raku_sym() -> crate::symbol::Symbol {
    static SYM: std::sync::OnceLock<crate::symbol::Symbol> = std::sync::OnceLock::new();
    *SYM.get_or_init(|| crate::symbol::Symbol::intern("raku"))
}

impl Interpreter {
    /// The `(repr)` suffix naming `val` in a type-check message, or `""` when it
    /// has none. A value the pure renderer can spell (numbers, strings, `Pair`s,
    /// `Range`s, `List`s, `Array`s, `Hash`es, `Set`s, ...) is answered by
    /// `value_short_repr`; an `Instance` is asked for its `.raku`, and a `Sub` or
    /// a container holding an object goes through the same leaf dispatch `.raku`
    /// itself uses (`raku_element_repr`).
    // Cost: O(t) plus the cost of the dispatched `.raku` calls, t = length of the
    // value's `.raku` text; the text kept is cut to a constant length by
    // `short_repr_of_raku`. Rakudo renders the whole `.raku` too.
    pub(crate) fn type_check_got_repr(&mut self, val: &Value) -> String {
        let val = &utils::decont_for_repr(val);
        if let ValueView::LazyList(ll) = val.view()
            && Self::lazy_seq_raku_applies(&ll)
            && let Ok(text) = self.lazy_seq_raku(&ll)
        {
            return utils::short_repr_of_raku(&text);
        }
        if !needs_raku_dispatch(val) && !container_needs_raku_dispatch(val) {
            return utils::value_short_repr(val);
        }
        if !matches!(val.view(), ValueView::Instance { .. }) {
            // A `Sub`, or a container holding an object: the pure container
            // rules with each dispatch-needing leaf rendered by its own `.raku`.
            return utils::short_repr_of_raku(&self.raku_element_repr(val));
        }
        // A `raku` the program declared wins; only a class without one gets the
        // built-in attribute dump. A user method that dies still yields the
        // default text: the message must name the type mismatch, not that.
        let user = match self.try_dispatch_compiled_method_direct(val, "raku", &[]) {
            Some(Ok(text)) => Some(text),
            _ => None,
        };
        // A built-in object type keeps its `.raku` in a native method handler,
        // not in the user-class attribute dump below (#10677): the pure table
        // for a `Blob`/`Buf`, the native instance handlers for an `IO::Path`.
        let text = user
            .or_else(
                || match crate::builtins::native_method_0arg(val, raku_sym()) {
                    Some(Ok(text)) => Some(text),
                    _ => None,
                },
            )
            .or_else(|| self.native_instance_raku(val))
            .or_else(|| match self.default_instance_repr(val, "raku", &[]) {
                Some(Ok(text)) => Some(text),
                _ => None,
            })
            .map(|text| text.to_string_value())
            .unwrap_or_else(|| raku_value(val));
        utils::short_repr_of_raku(&text)
    }

    /// `.raku` from the native instance handlers of a built-in class
    /// (`IO::Path`, ...), when the class has one.
    fn native_instance_raku(&mut self, val: &Value) -> Option<Value> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = val.view()
        else {
            return None;
        };
        let class_name = class_name.resolve();
        if !self.is_native_method(&class_name, "raku") {
            return None;
        }
        self.call_native_instance_method(&class_name, &attributes.as_map(), "raku", Vec::new())
            .ok()
    }

    /// The `X::TypeCheck::Assignment` for storing `val` into `var_name`
    /// (constraint `expected`), its message carrying the object's real `.raku`.
    pub(crate) fn type_check_assignment_failure(
        &mut self,
        var_name: &str,
        expected: &str,
        val: &Value,
    ) -> RuntimeError {
        let repr = self.type_check_got_repr(val);
        utils::type_check_assignment_typed_error_with_repr(var_name, expected, val, &repr)
    }

    /// The `X::TypeCheck::Binding` for `:=` binding `val` to a variable typed
    /// `expected`, its message carrying the object's real `.raku`.
    pub(crate) fn type_check_binding_failure(
        &mut self,
        expected: &str,
        val: &Value,
    ) -> RuntimeError {
        let repr = self.type_check_got_repr(val);
        utils::type_check_binding_typed_error(expected, val, &repr)
    }

    /// The `X::TypeCheck::Binding::Parameter` for binding `value` to the routine
    /// parameter `param` (constraint `expected`), its message carrying the
    /// object's real `.raku`.
    pub(crate) fn typecheck_binding_parameter_failure(
        &mut self,
        param: &str,
        expected: &str,
        value: &Value,
    ) -> RuntimeError {
        let repr = self.type_check_got_repr(value);
        RuntimeError::typecheck_binding_parameter_with_repr(param, expected, value, &repr)
    }

    /// The `X::TypeCheck::Assignment` for storing `val` as an element of the typed
    /// container `var_name` (`Type check failed for an element of @a; ...`), its
    /// message carrying the object's real `.raku`.
    pub(crate) fn type_check_element_failure(
        &mut self,
        var_name: &str,
        expected: &str,
        val: &Value,
    ) -> RuntimeError {
        let repr = self.type_check_got_repr(val);
        utils::type_check_element_typed_error_with_repr(var_name, expected, val, &repr)
    }

    /// [`Self::type_check_assignment_failure`] for the store paths that name the
    /// target optionally (`symbol`, e.g. `$!r`) and carry no variable name at all
    /// when stored through an alias: `RuntimeError::typecheck_assignment_with_repr`
    /// with the object's real `.raku` in its message.
    pub(crate) fn typecheck_assignment_failure(
        &mut self,
        expected: &str,
        val: &Value,
        symbol: Option<&str>,
    ) -> RuntimeError {
        let repr = self.type_check_got_repr(val);
        RuntimeError::typecheck_assignment_with_repr(expected, val, symbol, &repr)
    }
}
