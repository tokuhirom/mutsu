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

/// What a `but`-mixed value means for one 0-argument call.
pub(crate) enum MixinStep {
    /// The interpreter answers (a role mixin that must deep-copy its cell, or
    /// declares `of`/`keyof`; a plain allomorph's own row).
    Decline,
    /// The mixin's override, or the call answered on the wrapped value with the
    /// mixin put back.
    Answer(Result<Value, RuntimeError>),
    /// Ask the call of the wrapped value.
    Delegate(Value),
}

/// The step for `method` called with no argument on a `but`-mixed value, or
/// `None` when `target` is none. The order is Rakudo's: a plain allomorph's row
/// first (it is the table's), then what the mixin overrides (`Bool`, `Str`,
/// the stringifying methods, a mixin key named after the method), then the
/// wrapped value's own method. A role mixin's rows came before this step, at
/// the entries (`mixin_role_owners_of`).
// Cost: O(k) for the k mixin keys scanned by name; a delegated or cloned call
// adds the wrapped value's own cost.
pub(crate) fn mixin_step(target: &Value, method_sym: Symbol) -> Option<MixinStep> {
    use crate::builtins::methods_0arg::{native_method_0arg, raku_repr};
    let ValueView::Mixin(inner, mixins) = target.view() else {
        return None;
    };
    let method = method_sym.as_str();
    let has_role = || mixins.keys().any(|k| k.starts_with("__mutsu_role__"));
    // A plain allomorph's method with a row is the row's (§9.101).
    if crate::builtins::method_table::allomorph::answers(target, method_sym, &[]) {
        return Some(MixinStep::Decline);
    }
    if method == "Bool"
        && let Some(bool_val) = mixins.get("Bool")
    {
        return Some(MixinStep::Answer(Ok(bool_val.clone())));
    }
    if (method == "Str" || method == "~")
        && let Some(str_val) = mixins.get("Str")
    {
        return Some(MixinStep::Answer(Ok(str_val.clone())));
    }
    // A Bool with a mixed-in Bool override and no `Str` or role mixin
    // (`True but False`) is that Bool for every method, as in Rakudo (`.Str`,
    // `.Int`, `.not` and `.raku` all follow the effective truth, §9.111).
    if matches!(inner.as_ref().view(), ValueView::Bool(_))
        && mixins.get("Str").is_none()
        && !has_role()
        && let Some(ValueView::Bool(b)) = mixins.get("Bool").map(Value::view)
    {
        return Some(MixinStep::Delegate(Value::truth(b)));
    }
    // An allomorph gists as its preserved source string (`<1e3>.gist` is
    // `1e3`), and a `Str` mixed into any value (`5 but "x"`) wins for the
    // stringifying methods too: `.gist` and `.raku`/`.perl` answer it.
    if matches!(method, "gist" | "raku")
        && let Some(str_val) = mixins.get("Str")
        && matches!(str_val.view(), ValueView::Str(_))
        && (method == "gist" || crate::value::types::allomorph_type_name(inner, mixins).is_none())
    {
        return Some(MixinStep::Answer(Ok(str_val.clone())));
    }
    // An allomorph's `wordcase` reads the Str part but keeps the allomorph
    // type; its other string methods are `Str`'s and `Cool`'s rows on the Str
    // part (§9.101).
    if method == "wordcase"
        && let Some(str_val) = mixins.get("Str")
    {
        let wordcased = crate::value::wordcase_str(&str_val.to_string_value());
        return Some(MixinStep::Answer(Ok(
            crate::value::types::allomorph_wordcase_result(inner, wordcased),
        )));
    }
    // A Set/Bag/Mix names its own type in `.raku` and `.gist`, so a `but`-mixed
    // one names the role with it (`Set+{R}.new("a")`, `Set+{R}(a)`); only the
    // wrapper knows the mixed name.
    if matches!(method, "raku" | "gist") && crate::value::role_mixin_suffix(mixins).is_some() {
        let type_name = crate::value::what_type_name(target);
        let rendered = if method == "gist" {
            crate::runtime::utils::setbagmix_gist_named(inner, Some(&type_name))
        } else {
            raku_repr::setbagmix_raku_named(inner, Some(&type_name))
        };
        if let Some(rendered) = rendered {
            return Some(MixinStep::Answer(Ok(Value::str(rendered))));
        }
    }
    // A type object (`Any but role Meows {...}`) names its composed type in
    // `.gist`/`.raku`, as a Set/Bag/Mix does above; it has no user-class
    // dispatch below it to retarget the name.
    if matches!(method, "raku" | "gist")
        && matches!(inner.view(), ValueView::Package(_))
        && crate::value::role_mixin_suffix(mixins).is_some()
    {
        // A type object renamed by `.^set_name` renders by that name.
        let composed = mixins
            .get("__mutsu_type_name__")
            .map(Value::to_string_value)
            .unwrap_or_else(|| crate::value::what_type_name(target));
        let rendered = if method == "gist" {
            format!("({composed})")
        } else {
            composed
        };
        return Some(MixinStep::Answer(Ok(Value::str(rendered))));
    }
    // `^name` is deliberately not answered here (ADR-0060): a rename can live
    // on the composition-keyed shared `.WHAT` node, which needs the interpreter.
    //
    // `.clone` of a mixed-in value keeps the mixin (`(5 but False).clone` stays
    // `Int+{...}`; a Match, modelled as a string carrying its match state as a
    // mixin, stays a Match): clone the inner and put the mixins back. A role
    // mixin or an `:attr(val)` override needs the interpreter-backed clone so
    // the role's private cell is deep-copied.
    if method == "clone" {
        if mixins
            .keys()
            .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            return Some(MixinStep::Decline);
        }
        let inner_clone = match native_method_0arg(inner, method_sym) {
            Some(Ok(v)) => v,
            Some(Err(e)) => return Some(MixinStep::Answer(Err(e))),
            None => inner.as_ref().clone(),
        };
        return Some(MixinStep::Answer(Ok(Value::mixin_with_state(
            inner_clone,
            mixins.as_ref().clone(),
        ))));
    }
    // A role mixed in may parameterize the element or key type
    // (`%h but Associative[Int, Int]`) or declare `of` itself: the
    // interpreter's mixin dispatch reads the composition (§9.86).
    if matches!(method, "of" | "keyof") && has_role() {
        return Some(MixinStep::Decline);
    }
    // A mixin key named after the method: `True but [1, 2]` answers `.Array`.
    if let Some(mixin_val) = mixins.get(method) {
        return Some(MixinStep::Answer(Ok(mixin_val.clone())));
    }
    // `.flat` runs on the whole mixin: `flat_val` flattens through a container
    // inner while preserving a non-container inner's composition.
    if method == "flat" {
        let mut result = Vec::new();
        crate::builtins::flat_val(
            &crate::builtins::deitemize_flat_operand(target),
            &mut result,
            true,
        );
        return Some(MixinStep::Answer(Ok(Value::seq(result))));
    }
    // A role mixin over a SCALAR is itself the single element of its
    // `.list`/`.List`/`.Array` (`(5 but R).list[0].r`); only an aggregate's
    // mixin folds into its elements.
    if matches!(method, "list" | "List" | "Array") {
        let result = native_method_0arg(inner, method_sym);
        if let Some(Ok(res)) = &result
            && let ValueView::Array(items, kind) = res.view()
            && items.len() == 1
            && items
                .iter()
                .next()
                .is_some_and(|e| e.deref_container() == *inner.as_ref())
        {
            return Some(MixinStep::Answer(Ok(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(vec![target.clone()])),
                kind,
            ))));
        }
        return Some(match result {
            Some(answer) => MixinStep::Answer(answer),
            None => MixinStep::Decline,
        });
    }
    // A collection's `raku` row needs the interpreter (a typed container names
    // its type); a mixed-in one renders its own source form, the `$` of an
    // itemized inner included.
    if method == "raku"
        && matches!(
            inner.as_ref().view(),
            ValueView::Array(..) | ValueView::Seq(_) | ValueView::Hash(..) | ValueView::Pair(..)
        )
    {
        return Some(
            match crate::builtins::method_table::collection_render_names::raku(inner, &[]) {
                Some(answer) => MixinStep::Answer(answer),
                None => MixinStep::Decline,
            },
        );
    }
    Some(MixinStep::Delegate(inner.as_ref().clone()))
}
