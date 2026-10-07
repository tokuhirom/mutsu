//! A user class that `does Sequence` and supplies its own `iterator` answers
//! the list-ish protocol (`.list`, `.List`, `.Str`, `.elems`, ...) through that
//! iterator, as rakudo's `Sequence` role does: every one of those methods is
//! written in terms of `self.iterator`. Without it the instance was an opaque
//! single value (`SeqSplitter`, whose `.List` returned `SeqSplitter<..>`).
//!
//! The delegation drains the iterator into a `List` and re-targets the existing
//! native dispatch at it, like the QuantHash/Hash twins.

use super::*;

impl Interpreter {
    fn is_sequence_role_method(method: &str) -> bool {
        matches!(
            method,
            "list"
                | "List"
                | "cache"
                | "Seq"
                | "Array"
                | "array"
                | "Slip"
                | "elems"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "join"
                | "sort"
                | "reverse"
                | "sum"
                | "min"
                | "max"
                | "map"
                | "grep"
                | "first"
                | "head"
                | "tail"
        )
    }

    /// Whether `value` is an instance of a `does Sequence` class that supplies
    /// its own `iterator` (a method or a `has $.iterator` accessor), i.e. one
    /// whose `.Str`/`.list`/... [`Self::try_sequence_role_delegate`] answers.
    // Cost: O(1) amortized, memoized class probes.
    pub(crate) fn is_iterator_sequence_instance(&mut self, value: &Value) -> bool {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = value.view()
        else {
            return false;
        };
        let cn = class_name.resolve();
        self.class_does_role(&cn, "Sequence")
            && (self.has_user_method(&cn, "iterator") || attributes.contains_key("iterator"))
    }

    // Cost: O(n) to drain the iterator, n = items the iterator yields.
    pub(crate) fn try_sequence_role_delegate(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let method = method_sym.as_str();
        if !Self::is_sequence_role_method(method) {
            return None;
        }
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        let cn = class_name.resolve();

        // `iterator` is either a method or a `has $.iterator` accessor.
        if !self.class_does_role(&cn, "Sequence")
            || !(self.has_user_method(&cn, "iterator") || attributes.contains_key("iterator"))
        {
            return None;
        }
        if self.has_user_method_including_role(&cn, method) {
            return None;
        }
        // A `has $.iterator` accessor is read directly; the Sequence role's own
        // `iterator` would wrap the instance itself.
        let items = if self.has_user_method(&cn, "iterator") {
            self.drive_user_iterator_items(target)
        } else {
            let iterator = attributes
                .as_map()
                .get("iterator")
                .cloned()
                .unwrap_or(Value::NIL);
            self.drive_iterator_value_items(iterator)
                .map(|(items, advanced)| {
                    // The cursor lives in the iterator: keep the moved one, so a
                    // second `.List` continues where the first stopped.
                    attributes.insert("iterator".to_string(), advanced);
                    items
                })
        };
        let items = match items {
            Ok(items) => items,
            Err(e) => return Some(Err(e)),
        };
        let list = Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(items)),
            crate::value::ArrayKind::List,
        );
        if let Some(r) = self.try_native_method(&list, method_sym, args) {
            return Some(r);
        }
        Some(self.try_compiled_method_or_interpret_sym(list, method_sym, args.to_vec()))
    }
}
