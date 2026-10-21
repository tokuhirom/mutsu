//! The receiver-normalization step every dispatch entry runs before it asks
//! the method table (ADR-11276 §2.4, #14080).
//!
//! A method call reads its invocant through a few receiver states the rows do
//! not see: a `Scalar` container (or an itemized `Array`/`Hash`) is
//! transparent except for the methods that observe the container itself, a
//! hash-like receiver and a `Match` answer `Any`'s `self.list.METHOD` methods
//! on their list, a member of a `my Array enum` answers its base value's
//! collection methods, and an instance of a user `Channel` subclass coerces
//! the channel it carries. Each entry used to spell these steps itself (the
//! VM's `try_native_method`, the by-name `call_method_with_values`, and the
//! prologues of the 0- and 1-argument cascades), and the copies drifted.
//! [`step`] decides them once; an entry only acts on the answer, through its
//! own recursion and its own renderer.
//!
//! [`step`] is pure. The checks that need the interpreter -- a user method of
//! the receiver's own class that overrides `Any`'s list method, and the `Cool`
//! methods of a user `IO::Path` subclass, which read the class's MRO -- are
//! [`step_in`]'s.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// What a receiver state means for one call.
pub(crate) enum Step {
    /// Ask the call of this receiver instead.
    Redirect(Value, Why),
    /// The call's answer, which needs no row (`.VAR` of a container, a
    /// `Channel` subclass's list coercion, a `Match`'s `.List`).
    Answer(Result<Value, RuntimeError>),
    /// `.raku` (or `.item`/`.self`, or `.VAR` of an itemized aggregate) of a
    /// `$`-container: the container itself is the receiver, which the entry
    /// answers with its own renderer or descriptor.
    Itself,
}

/// Why a receiver is redirected.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Why {
    /// A container's value: every method but the container's own reads it.
    Decont,
    /// `Any`'s `self.list.METHOD` (a hash-like receiver's pairs, a `Match`'s
    /// positional captures): a method the receiver's own class declares wins.
    SelfList,
    /// Another value standing in for the receiver (an enum member's base
    /// value, a user `IO::Path` subclass's path).
    Delegate,
}

/// The methods of a `$`-container that observe the container itself.
// Cost: O(1).
fn observes_container(method: &str) -> bool {
    matches!(method, "raku" | "perl" | "item" | "self")
}

/// The receiver step for `method` called on `target` with `argc` positional
/// arguments, or `None` when `target` is read as it stands.
// Cost: O(1) tag probes for a receiver in no such state; O(e) to build a
// hash-like receiver's pairs (e = entries) or a Match's positional list.
pub(crate) fn step(target: &Value, method: &str, argc: usize) -> Option<Step> {
    match target.view() {
        ValueView::Scalar(inner) => {
            if method == "VAR" {
                return Some(Step::Answer(Ok(Value::package(Symbol::intern("Scalar")))));
            }
            if observes_container(method) && method != "item" && method != "self" {
                return Some(Step::Itself);
            }
            // A scalar shared with an array/hash variable is
            // `Scalar(ContainerRef(cell))`: every ordinary method reads
            // through the cell (`$shared.push` reaches the backing container).
            let inner = if inner.is_container_ref() {
                inner.deref_container()
            } else {
                inner.clone()
            };
            Some(Step::Redirect(inner, Why::Decont))
        }
        // An itemized Array/List is a `Scalar` container too: the kind flip
        // keeps the same backing data, so a mutator still writes through.
        ValueView::Array(items, kind) if kind.is_itemized() => {
            // `.VAR` of an itemized aggregate is the container's own question:
            // the VM's variable descriptor answers it where the call came from
            // a variable, and the by-name entry answers `Scalar` otherwise.
            if method == "VAR" || observes_container(method) {
                return Some(Step::Itself);
            }
            Some(Step::Redirect(
                Value::array_with_kind(items.clone(), kind.decontainerize()),
                Why::Decont,
            ))
        }
        // The Hash half: its itemization is a flag on the same `HashData`
        // (ADR-0040), cleared over the same `Gc` (#10744).
        ValueView::Hash(_) if target.hash_is_itemized() => {
            if method == "VAR" || observes_container(method) {
                return Some(Step::Itself);
            }
            Some(Step::Redirect(
                target.clone().with_hash_itemized(false),
                Why::Decont,
            ))
        }
        _ => list_step(target, method, argc),
    }
}

/// The steps of a receiver that is no container.
// Cost: as [`step`].
fn list_step(target: &Value, method: &str, argc: usize) -> Option<Step> {
    // `Hash`/`Map` and the QuantHashes inherit `reverse`/`unique`/`squish`/...
    // from `Any`, which defines each as `self.list.METHOD` (#10758).
    if let Some(pairs) = crate::runtime::utils::hashlike_receiver_as_pairs_list(target, method) {
        return Some(Step::Redirect(pairs, Why::SelfList));
    }
    // A `Match` answers them on its positional captures (§9.98); `List` and
    // `Hash` are that list or map itself.
    if let Some(list) = target.match_list_receiver(method) {
        if argc == 0 && crate::value::match_view::match_list_answers_itself(method) {
            return Some(Step::Answer(Ok(list)));
        }
        return Some(Step::Redirect(list, Why::SelfList));
    }
    if let Some(base) = Interpreter::enum_aggregate_base(target, method) {
        return Some(Step::Redirect(base, Why::Delegate));
    }
    // A `Channel` subclass's list coercions read its channel (#13634).
    Interpreter::channel_subclass_coercion(target, method).map(Step::Answer)
}

/// Whether [`step`] takes `method` on `target` away from the receiver as it
/// stands (a redirect or an answer of its own), without building the
/// redirected receiver: the table's entry declines such a call, so the entry
/// that asked it runs the step instead and no row answers the receiver as
/// one item (`\(1, 2).unique` reads the capture's list).
// Cost: O(1) tag probes and short name scans; a `Backtrace` copies its frames.
pub(crate) fn claims(target: &Value, method: &str) -> bool {
    // A lazy Match is asked by its tag: `view()` would materialize it, and
    // its accessors (`from`, `to`, `Str`, ...) are the grammar engine's hot
    // calls.
    if target.is_lazy_match_value() {
        return method == "Hash" || crate::value::match_view::is_capture_list_method(method);
    }
    match target.view() {
        ValueView::Scalar(_) => !matches!(method, "raku" | "perl"),
        ValueView::Array(_, kind) if kind.is_itemized() => {
            method != "VAR" && !observes_container(method)
        }
        ValueView::Hash(_) if target.hash_is_itemized() => {
            method != "VAR" && !observes_container(method)
        }
        ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            crate::runtime::utils::is_hashlike_any_list_method(method)
        }
        ValueView::Capture { .. } => {
            method == "Hash" || crate::value::match_view::is_capture_list_method(method)
        }
        ValueView::Enum { .. } => Interpreter::enum_aggregate_base(target, method).is_some(),
        ValueView::Instance { .. } => {
            target.match_list_receiver(method).is_some()
                || (crate::builtins::method_table::CHANNEL_LIST_COERCIONS.contains(&method)
                    && crate::builtins::method_table::promise_channel_receiver_of(target)
                        .is_some_and(|r| matches!(r.view(), ValueView::Channel(_))))
        }
        _ => target.is_match_instance() && target.match_list_receiver(method).is_some(),
    }
}

/// [`step`], with the checks that need the interpreter: a `self.list` redirect
/// yields to a method the receiver's own class (an `augment`, a grammar)
/// declares, and a `Cool` method of a user `IO::Path` subclass reads its path
/// (#12149).
// Cost: as [`step`], plus a memoized method probe for a `self.list` redirect
// and, for an instance, `io_path_subclass_cool_receiver`'s.
pub(crate) fn step_in(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    argc: usize,
) -> Option<Step> {
    if let Some(path) = interp.io_path_subclass_cool_receiver(target, method) {
        return Some(Step::Redirect(path, Why::Delegate));
    }
    match step(target, method, argc)? {
        Step::Redirect(_, Why::SelfList) | Step::Answer(_)
            if self_list_overridden(interp, target, method) =>
        {
            None
        }
        step => Some(step),
    }
}

/// Whether the receiver's own class declares `method`, which then wins over
/// `Any`'s `self.list.METHOD`.
// Cost: one memoized method probe.
fn self_list_overridden(interp: &mut Interpreter, target: &Value, method: &str) -> bool {
    if target.is_match_instance() {
        return interp.has_user_method(target.match_dispatch_class(), method);
    }
    matches!(
        target.view(),
        ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
    ) && interp.has_user_method(crate::runtime::utils::value_type_name(target), method)
}

/// Whether only the general (by-name) entry, `call_method_with_values`,
/// answers `method` on `target`: a receiver state the rows and the pure
/// cascades cannot read, so a native entry declines the call before it asks
/// either. The states (ADR-11276 slice 5, #11276):
///
/// - a `Proxy`, which the general entry FETCHes (its identity methods are the
///   container's own);
/// - a receiver whose elements live behind an iterator the rows cannot pull:
///   an `IterationBuffer`, a user instance or a role mixin with its own
///   `iterator` (and no method of this name), for the iteration methods;
/// - a lazy source a `map`/`grep` turns into a pipeline stage;
/// - `pick`/`roll` of an enum type object (or its name), whose values only the
///   enum registry knows;
/// - `.raku` of a typed or `Map`-declared `Hash`, which names its type.
// Cost: O(1) name and tag probes; for an instance or mixin with an iteration
// name, two memoized method probes; for a typed `Hash.raku`, one metadata
// lookup.
pub(crate) fn general_entry_owns(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    argc: usize,
) -> bool {
    if target.is_proxy_value() {
        return !matches!(
            method,
            "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
        );
    }
    if matches!(
        method,
        "grep" | "map" | "first" | "sort" | "head" | "tail" | "flat"
    ) && iterates_elsewhere(interp, target, method)
    {
        return true;
    }
    if matches!(method, "map" | "grep")
        && target.is_lazy_list_value()
        && Interpreter::is_lazy_pipe_source(target)
    {
        return true;
    }
    if matches!(method, "pick" | "roll") && argc <= 1 {
        return match target.view() {
            ValueView::Package(name) => interp.registry().enum_types.contains_key(name.as_str()),
            ValueView::Str(name) => interp.registry().enum_types.contains_key(name.as_str()),
            _ => false,
        };
    }
    method == "raku"
        && argc == 0
        && !target.is_lazy_match_value()
        && matches!(target.view(), ValueView::Hash(_))
        && interp.container_type_metadata(target).is_some()
}

/// Whether `target`'s elements are pulled through an iterator the rows cannot
/// run for `method`.
// Cost: O(1) tag probes, plus two memoized method probes for an instance or
// a mixin.
fn iterates_elsewhere(interp: &mut Interpreter, target: &Value, method: &str) -> bool {
    if crate::runtime::nqp_ops_list::is_iteration_buffer(target) {
        return true;
    }
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if !attributes.contains_key("__mutsu_array_storage")
            && !attributes.contains_key("__mutsu_hash_storage") =>
        {
            interp.has_user_method(class_name.as_str(), "iterator")
                && !interp.has_user_method(class_name.as_str(), method)
        }
        ValueView::Mixin(..) => {
            interp.mixin_composes_method(target, "iterator")
                && !interp.mixin_composes_method(target, method)
        }
        _ => false,
    }
}
