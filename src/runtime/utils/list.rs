use super::*;
use crate::value::ValueView;

/// Coerce a list element to a form that binds *positionally* when passed as the
/// topic/argument of a matcher or comparator block.
///
/// mutsu uses the `Value` variant to distinguish a call-site named argument
/// (`Pair`, excluded from positional arity everywhere in dispatch) from a
/// positional pair *value* (`ValuePair`). Iterating a Hash yields
/// `Pair` elements, so passing such an element straight to a block (e.g.
/// `%h.first({ .value > 1 })` / `%h.sort({ ... })`) would bind it as a *named*
/// argument, leaving the block with zero positionals ("expected N got 0"). Map
/// `Pair` to `ValuePair` so the element binds as `$_`/`$^a`.
pub(crate) fn pair_as_positional(val: &Value) -> Value {
    match val.view() {
        ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
        _ => val.clone(),
    }
}

/// Decompose a method-call RECEIVER into its own elements, ignoring the
/// receiver's own itemization (a `Scalar` wrapper, an itemized `ArrayKind`,
/// or a hash's itemized flag). `value_to_list`'s itemization checks answer
/// "does this value flatten when it is an ELEMENT of some other container"
/// (ADR-0040) — a different question from "what are MY OWN elements",
/// which is what `.pick`/`.roll`/`.hyper`/`.race`/etc. need when `val` is
/// the receiver they were called on. Reusing `value_to_list` unmodified for
/// that purpose silently "rolls"/"picks" the whole itemized value instead
/// of one of its elements (e.g. `%h<a>.roll` on a nested-autovivified,
/// itemized `%h<a>` returned the Hash itself instead of a random Pair).
pub(crate) fn value_to_list_for_receiver(val: &Value) -> Vec<Value> {
    let bare = val.descalarize();
    // A Uni/NFC/NFD/NFKC/NFKD value has no itemization wrapper of its own
    // (unlike Array/Hash) -- it is always its own receiver, and its OWN
    // elements are its codepoints, each a plain Int. `value_to_list` (used
    // for list-CONTEXT flattening, e.g. a `for $n -> $c` single-argument
    // scalar) intentionally keeps treating a bare Uni as one item; this
    // receiver-specific function is where `.map`/`.grep`/`.sort`/`.pick`/...
    // decompose it, matching Rakudo (`'ab'.NFC.map({...})` runs the
    // callback once per codepoint, each bound as a real Int).
    if let ValueView::Uni(u) = bare.view() {
        return u
            .codepoints()
            .into_iter()
            .map(|cp| Value::int(cp as i64))
            .collect();
    }
    // A `Match` (a `Capture`) is not `Iterable`, but `Any.iterator` is
    // `self.list.iterator` and `Capture.list` is the positional part, so its
    // OWN elements are its positional captures: raku's
    // `('ab' ~~ /(.)(.)/).map(*.Str)` is `("a", "b")`, not the whole match.
    if bare.is_match_instance() {
        // An unbound interior slot iterates as `Mu` (`match_list_view`).
        return match bare.match_list() {
            Some(list) => value_to_list(&list)
                .into_iter()
                .map(Value::unbound_capture_as_mu)
                .collect(),
            None => Vec::new(),
        };
    }
    let bare = match bare.view() {
        ValueView::Array(items, kind) if kind.is_itemized() => {
            Value::array_with_kind(items.clone(), kind.decontainerize())
        }
        ValueView::Hash(_) if bare.hash_is_itemized() => bare.clone().with_hash_itemized(false),
        _ => bare.clone(),
    };
    value_to_list(&bare)
}

/// The list-iteration methods `Hash`/`Map` and the QuantHashes (`Set`/`Bag`/
/// `Mix` and their mutable forms) inherit from `Any`, which defines each as
/// `self.list.METHOD`: on such a receiver the invocant IS its list of Pairs.
/// (`Any.reverse`, `Any.unique`, `Any.squish`, `Any.eager`, `Any.Supply`,
/// `Any.minmax` and `Any.produce` all go through `self.list`.)
/// Methods that already have an arm of their own for these receivers (`keys`,
/// `values`, `kv`, `pairs`, `sort`, `map`, `grep`, `first`, `tail`, ...) are
/// deliberately not listed, and neither is `Seq`, whose single shared
/// implementation (`builtins::seq_coerce::to_seq_structural`) has its own arm.
const HASHLIKE_ANY_LIST_METHODS: &[&str] = &[
    "reverse", "unique", "squish", "eager", "minmax", "produce", "Supply",
];

/// For a `Hash`/`Map`/`Set`/`Bag`/`Mix` invocant of one of
/// [`HASHLIKE_ANY_LIST_METHODS`], the `List` of Pairs it stands for, so the
/// caller re-dispatches the SAME method on that list (`%h.unique` is
/// `%h.list.unique`; `bag(<a a b>).reverse` is `(:b(1), :a(2))`). `None` for
/// every other receiver or method, so the common path pays one `view()` probe.
///
/// The receiver's own itemization is ignored (a method call decontainerizes
/// its invocant), and an object hash / typed QuantHash yields its key OBJECTS,
/// because the pairs come from [`value_to_list_for_receiver`].
// Cost: O(1) for any other receiver or an unlisted method; O(e) for a hash-like
// receiver, e = entries (the Pairs are materialized once).
pub(crate) fn hashlike_receiver_as_pairs_list(target: &Value, method: &str) -> Option<Value> {
    if !matches!(
        target.view(),
        ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
    ) || !HASHLIKE_ANY_LIST_METHODS.contains(&method)
    {
        return None;
    }
    Some(Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(value_to_list_for_receiver(
            target,
        ))),
        crate::value::ArrayKind::List,
    ))
}

/// True when a subscript range dimension has no usable finite end: `1..*` /
/// `1^..Inf` style (a Whatever end lowers to `Inf`, and an integer-range end of
/// `i64::MAX` is the same thing forced through an int range). Expanding such a
/// range eagerly would allocate ~2^63 elements; a subscript instead clamps it
/// to the axis length (see `expand_unbounded_range_dim`).
pub(crate) fn subscript_range_end_unbounded(dim: &Value) -> bool {
    match dim.view() {
        ValueView::Range(_, b)
        | ValueView::RangeExcl(_, b)
        | ValueView::RangeExclStart(_, b)
        | ValueView::RangeExclBoth(_, b) => b == i64::MAX,
        ValueView::GenericRange { end, .. } => match end.as_ref().view() {
            ValueView::Whatever => true,
            ValueView::Int(i) => i == i64::MAX,
            ValueView::Num(f) => f.is_infinite() && f.is_sign_positive(),
            ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => n > 0,
            _ => false,
        },
        _ => false,
    }
}

/// Expand an unbounded-end subscript range dimension against a known axis
/// length: `@a[1^..*;1]` selects rows 2..len-1 at that level. Only
/// unbounded-end ranges are handled — a bounded range keeps the generic
/// expansion (preserving its out-of-bounds Nil semantics). Returns None for
/// non-ranges, bounded ranges, or a non-integer start.
pub(crate) fn expand_unbounded_range_dim(dim: &Value, len: usize) -> Option<Vec<Value>> {
    if !subscript_range_end_unbounded(dim) {
        return None;
    }
    let (start, excl_start) = match dim.view() {
        ValueView::Range(a, _) | ValueView::RangeExcl(a, _) => (a, false),
        ValueView::RangeExclStart(a, _) | ValueView::RangeExclBoth(a, _) => (a, true),
        ValueView::GenericRange {
            start, excl_start, ..
        } => match start.as_ref().view() {
            ValueView::Int(i) => (i, excl_start),
            ValueView::Num(f) if f.fract() == 0.0 => (f as i64, excl_start),
            ValueView::Rat(n, d) if d != 0 && n % d == 0 => (n / d, excl_start),
            _ => return None,
        },
        _ => return None,
    };
    let start = if excl_start { start + 1 } else { start };
    let start = start.max(0);
    if len == 0 || start >= len as i64 {
        return Some(Vec::new());
    }
    Some((start..len as i64).map(Value::int).collect())
}
