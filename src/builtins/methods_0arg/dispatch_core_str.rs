/// String and text methods: words, codes, lines, trim, trim-leading, trim-trailing,
/// flip, so, not, is-lazy, lazy, chomp, chop, comb, fmt, join
use crate::value::{RuntimeError, Value, ValueView};

use super::is_value_lazy;

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        // Cost: O(1), a lazy Seq over the invocant (`crate::value::StrIterSpec`).
        "lines" => {
            // Skip for Supply instances -- handled by native Supply.lines
            if let ValueView::Instance { class_name, .. } = target.view()
                && (class_name == "Supply"
                    || class_name == "IO::Handle"
                    || class_name == "IO::Path"
                    || class_name == "IO::Socket::INET")
            {
                return Some(None);
            }
            Some(Some(crate::builtins::method_table::str_iter::lines(
                target,
                &[],
            )))
        }
        "so" => {
            // Calling .so on a Failure marks it as handled
            if let ValueView::Instance { class_name, .. } = target.view()
                && class_name == "Failure"
            {
                target.mark_failure_handled();
            }
            Some(Some(Ok(Value::truth(target.truthy()))))
        }
        "not" => {
            // Calling .not on a Failure marks it as handled
            if let ValueView::Instance { class_name, .. } = target.view()
                && class_name == "Failure"
            {
                target.mark_failure_handled();
            }
            Some(Some(Ok(Value::truth(!target.truthy()))))
        }
        // Cost: O(1) (inspects the invocant's own lazy flag / range end only).
        "is-lazy" => {
            // For Iterator instances, check the stored is_lazy attribute
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
                && class_name == "Iterator"
            {
                let lazy = matches!(
                    attributes.as_map().get("is_lazy").map(Value::view),
                    Some(ValueView::Bool(true))
                );
                return Some(Some(Ok(Value::truth(lazy))));
            }
            // A consumed gather-based LazyList throws X::Seq::Consumed on .is-lazy
            if let ValueView::LazyList(ll) = target.view() {
                let is_gather = ll.env.get("__mutsu_lazylist_from_gather").is_some();
                if is_gather && crate::value::lazylist_is_consumed(&ll) {
                    return Some(Some(Err(crate::value::seq_consumed_error())));
                }
                // `is_genuinely_lazy` is the single authority for "is this
                // `.is-lazy`". The former hand-rolled predicate here answered
                // True for a plain `gather {...}` (raku: False) and for a
                // finite `.map` pipe.
                return Some(Some(Ok(Value::truth(ll.is_genuinely_lazy()))));
            }
            Some(Some(Ok(Value::truth(is_value_lazy(target)))))
        }
        "lazy" => {
            // A lazy `LazyList` is re-tagged so that assigning it to an array
            // keeps it lazy; every other receiver is the `lazy` rows'
            // implementation (`method_table::lazy`).
            if is_value_lazy(target)
                && let ValueView::LazyList(list) = target.view()
            {
                let mut env = list.env.clone();
                env.insert(
                    "__mutsu_preserve_lazy_on_array_assign".to_string(),
                    Value::TRUE,
                );
                let cache = list.cache.lock().unwrap().clone();
                return Some(Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                    crate::value::LazyList {
                        body: list.body.clone(),
                        env,
                        cache: std::sync::Mutex::new(cache),
                        generation_state: std::sync::Mutex::new(None),
                        compiled_code: list.compiled_code.clone(),
                        compiled_fns: list.compiled_fns.clone(),
                        elems_count: list.elems_count.clone(),
                        scan_spec: list
                            .scan_spec
                            .as_ref()
                            .map(|s| std::sync::Mutex::new(s.lock().unwrap().clone())),
                        sequence_spec: list.sequence_spec.clone(),
                        coroutine: list
                            .coroutine
                            .as_ref()
                            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
                        lazy_pipe: list
                            .lazy_pipe
                            .as_ref()
                            .map(|p| std::sync::Mutex::new(p.lock().unwrap().clone())),
                        closure_seq: list
                            .closure_seq
                            .as_ref()
                            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
                        walk_pending: list
                            .walk_pending
                            .as_ref()
                            .map(|w| std::sync::Mutex::new(w.lock().unwrap().clone())),
                        cat_pull: list
                            .cat_pull
                            .as_ref()
                            .map(|c| std::sync::Mutex::new(c.lock().unwrap().clone())),
                        array_context: list.array_context,
                        list_context: list.list_context,
                        cached_no_sink: list.cached_no_sink,
                        itemized: list.itemized,
                    },
                )))));
            }
            Some(crate::builtins::method_table::lazy::lazy(target, &[]))
        }
        // Cost: O(1) when nothing is chomped (see chomp_value); O(n) otherwise, n =
        // chars of the invocant.
        "chomp" => {
            // IO::Handle.chomp (and any IO::Handle-derived class, e.g.
            // Text::IO::String) is an attribute accessor, not the Str method.
            // This layer cannot see the MRO, so route EVERY instance to the
            // slow path — it dispatches user/parent methods and still reaches
            // the native Str chomp for Str-derived instances.
            if matches!(target.view(), ValueView::Instance { .. }) {
                return Some(None);
            }
            Some(Some(crate::builtins::method_table::str::chomp(target, &[])))
        }
        // Cost: O(n), n = chars of the invocant (result copied).
        "chop" => {
            if let ValueView::Package(type_name) = target.view() {
                return Some(Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                )))));
            }
            Some(Some(crate::builtins::method_table::str::chop(target, &[])))
        }
        // The `Any.join` row's implementation (`method_table::collections::join`).
        "join" => {
            use crate::builtins::method_table::collection_join::{Joined, join_core};
            match join_core(target, "") {
                Joined::Done(result) => Some(Some(result)),
                Joined::NeedsInterpreter => Some(None),
                // `.join` on a Thread is the thread-join primitive (block until
                // the thread finishes): the runtime's native_thread routes it.
                Joined::NotCovered if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Thread") => {
                    Some(None)
                }
                Joined::NotCovered => Some(Some(Ok(Value::str(target.to_string_value())))),
            }
        }
        _ => None,
    }
}
