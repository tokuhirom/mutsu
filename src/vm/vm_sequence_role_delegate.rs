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
                | "pairs"
                | "kv"
                | "keys"
                | "values"
                | "antipairs"
                | "flat"
                | "reduce"
                | "produce"
                | "unique"
                | "squish"
                | "rotor"
                | "batch"
                | "any"
                | "all"
                | "one"
                | "none"
                | "pick"
                | "roll"
                | "AT-POS"
                | "Numeric"
                | "classify"
                | "categorize"
                | "combinations"
                | "permutations"
                | "end"
        )
    }

    /// Whether `value` is an instance of a `does Sequence` class that supplies
    /// its own `iterator` (a method or a `has $.iterator` accessor), i.e. one
    /// whose `.Str`/`.list`/... [`Self::try_sequence_role_delegate`] answers.
    // Cost: O(1) amortized, memoized class probes.
    pub(crate) fn is_iterator_sequence_instance(&mut self, value: &Value) -> bool {
        // A `but` mixin keeps the instance (and so its `Sequence` role) inside.
        if let ValueView::Mixin(inner, _) = value.view() {
            let inner = (**inner).clone();
            return self.is_iterator_sequence_instance(&inner);
        }
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

    /// How many items `method(args)` of a `Sequence` needs from its iterator:
    /// `Some(n)` for a prefix consumer (`head(n)`, `head`, a bare `first`,
    /// `AT-POS(i)`), `None` when it needs the whole sequence. Rakudo's
    /// `Sequence` pulls these lazily, so an endless iterator still answers.
    // Cost: O(1).
    fn sequence_prefix_need(method: &str, args: &[Value]) -> Option<usize> {
        let int_arg = |v: &Value| match v.deref_container().view() {
            ValueView::Int(i) if i >= 0 => usize::try_from(i).ok(),
            _ => None,
        };
        match (method, args) {
            ("head", []) | ("first", []) => Some(1),
            ("head", [n]) => int_arg(n),
            ("AT-POS", [i]) => int_arg(i).and_then(|i| i.checked_add(1)),
            _ => None,
        }
    }

    // Cost: O(n) to drain the iterator, n = items the iterator yields; a
    // prefix consumer (`head(k)`, `AT-POS(i)`) pulls only that prefix, O(k).
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
        // A method call deconts its invocant: `$seq.item.list` reads the
        // sequence itself, not a one-element list holding it. The renderers
        // keep the container, which they show.
        let decont;
        let target = match target.view() {
            ValueView::Scalar(inner)
                if !matches!(method, "gist" | "raku")
                    && matches!(
                        inner.view(),
                        ValueView::Instance { .. } | ValueView::Mixin(..)
                    ) =>
            {
                decont = (*inner).clone();
                &decont
            }
            _ => target,
        };
        // Under a `but` mixin the wrapped instance decides, but `self` stays
        // the mixed value so the mixin's own methods (`of`, ...) still win
        // inside the user `iterator`.
        let probed = match target.view() {
            ValueView::Mixin(inner, _) => (**inner).clone(),
            _ => target.clone(),
        };
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = probed.view()
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
        // `Sequence.elems` / `.end` of a lazy sequence soft-fail with
        // X::Cannot::Lazy instead of pulling the endless iterator (ParaSeq
        // over `^Inf` answers `is-lazy` True).
        if matches!(method, "elems" | "end")
            && args.is_empty()
            && self.has_user_method_including_role(&cn, "is-lazy")
            && let Ok(lazy) = self.call_method_with_values(target.clone(), "is-lazy", Vec::new())
            && lazy.truthy()
        {
            return Some(Ok(RuntimeError::cannot_lazy_action_failure(&format!(
                ".{method}"
            ))));
        }
        // A `has $.iterator` accessor is read directly; the Sequence role's own
        // `iterator` would wrap the instance itself.
        let need = Self::sequence_prefix_need(method, args);
        // `map` / `grep` / `first(matcher)` are lazy in Rakudo's `Sequence`: a
        // lazy `Seq` over the class's own iterator takes the call, so a
        // following `head(n)` pulls only what it needs and an endless iterator
        // answers (#16133).
        if matches!(method, "map" | "grep") || (method == "first" && !args.is_empty()) {
            let iterator = if self.has_user_method(&cn, "iterator") {
                self.user_iterator_of(target)
            } else {
                Ok(attributes
                    .as_map()
                    .get("iterator")
                    .cloned()
                    .unwrap_or(Value::NIL))
            };
            let iterator = match iterator {
                Ok(iterator) => iterator,
                Err(e) => return Some(Err(e)),
            };
            let seq = self.try_native_seq_construct(&[iterator]);
            if method == "first" {
                // Stops pulling at the first match.
                return match self.first_over_map_grep_seq(&seq, method, args) {
                    Ok(Some(hit)) => Some(Ok(hit)),
                    Ok(None) => Some(self.dispatch_on_list_value(seq, method_sym, args)),
                    Err(e) => Some(Err(e)),
                };
            }
            return Some(self.dispatch_on_list_value(seq, method_sym, args));
        }
        let items = if self.has_user_method(&cn, "iterator") {
            if matches!(target.view(), ValueView::Mixin(..)) {
                // The class's own `iterator`, with the mixed value as `self`
                // (not the Sequence role's, which would wrap the instance).
                self.call_user_iterator_on_mixin(target)
                    .and_then(|iterator| self.drive_iterator_value_items_upto(iterator, need))
                    .map(|(items, _)| items)
            } else {
                self.drive_user_iterator_items_upto(target, need)
            }
        } else {
            let iterator = attributes
                .as_map()
                .get("iterator")
                .cloned()
                .unwrap_or(Value::NIL);
            self.drive_iterator_value_items_upto(iterator, need)
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
        Some(self.dispatch_on_list_value(list, method_sym, args))
    }

    /// `method(args)` on the list-ish `value` the sequence's items became:
    /// the native method first, then the compiled/interpreted dispatch.
    // Cost: that of the method called.
    fn dispatch_on_list_value(
        &mut self,
        value: Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(r) = self.try_native_method(&value, method_sym, args) {
            return r;
        }
        self.try_compiled_method_or_interpret_sym(value, method_sym, args.to_vec())
    }
}
