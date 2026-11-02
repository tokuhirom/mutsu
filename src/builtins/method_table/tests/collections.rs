//! Tests of the collections group (ADR-11276 §10, slice 3C).

use super::*;

#[test]
fn seq_state_rows_refuse_consumed_lazy_queries_without_a_cascade() {
    use crate::value::{SeqBody, SeqSource};
    let body = SeqBody::deferred(SeqSource::Iterator(Value::int(0)));
    assert!(body.take_iterator_source().is_ok());
    let target = Value::seq_body(body);
    for owner in ["Seq", "Mu"] {
        let answer = invoke_owner_pure(&[owner], &target, Symbol::intern("is-lazy"));
        assert!(
            matches!(answer, Some(Err(_))),
            "{owner}'s row must refuse a consumed Seq"
        );
    }
}

#[test]
fn seq_state_rows_keep_an_already_indexed_io_stream_without_a_cascade() {
    let target = Value::seq_deferred(crate::value::SeqSource::IoLines {
        handle: Value::int(0),
        words: false,
        kv: true,
    });
    let answer = invoke_owner_pure(&["Any"], &target, Symbol::intern("kv"));
    assert!(
        matches!(answer, Some(Ok(answer)) if matches!(
            (answer.view(), target.view()),
            (crate::value::ValueView::Seq(answer), crate::value::ValueView::Seq(original))
                if std::sync::Arc::ptr_eq(&answer, &original)
        )),
        "the VM's indexed stream must retain its body"
    );
}

#[test]
fn seq_state_rows_request_cache_without_a_cascade() {
    use crate::value::{SeqBody, SeqSource};
    let body = SeqBody::deferred(SeqSource::Iterator(Value::int(0)));
    let target = Value::seq_body(body.clone());
    assert!(!body.is_cached());
    let answer = invoke_owner_pure(&["Seq"], &target, Symbol::intern("cache"));
    assert!(body.is_cached());
    assert!(
        body.has_deferred_source(),
        "requesting cache must not pull its source"
    );
    assert!(matches!(answer, Some(Ok(answer))
        if crate::runtime::utils::value_type_name(&answer) == "List"));
}

#[test]
fn counted_head_tail_rows_resolve_for_every_plain_shape() {
    for name in ["head", "tail"] {
        for shape in DispatchShape::ALL
            .into_iter()
            .filter(|shape| shape.inherits())
        {
            // Rakudo declares `head` and `tail` on `List` (and `head` on `Map`), so the
            // rows of those owners answer first; every other shape reaches `Any`'s.
            let owner = match shape {
                DispatchShape::Array if name == "tail" => "Array",
                DispatchShape::List | DispatchShape::Array => "List",
                DispatchShape::Hash if name == "head" => "Map",
                _ => "Any",
            };
            assert_eq!(
                owner_of(shape, Symbol::intern(name), 1),
                owner,
                "{owner}.{name} should resolve for {shape:?}"
            );
        }
    }
}

/// `List`'s count rows reach `Array` through its MRO, and `Map`'s reach
/// `Hash`.
#[test]
fn count_rows_reach_their_subtypes() {
    for name in ["keys", "Numeric", "Int"] {
        let sym = Symbol::intern(name);
        assert_eq!(owner_of(DispatchShape::Array, sym, 0), "List");
        assert_eq!(owner_of(DispatchShape::List, sym, 0), "List");
        assert_eq!(owner_of(DispatchShape::Hash, sym, 0), "Map");
    }
}

#[test]
fn aggregate_rows_resolve_to_the_rakudo_owners() {
    for name in ["minmax", "sum"] {
        let sym = Symbol::intern(name);
        // `List.sum` is a row of its own (Rakudo declares it there); `minmax` is `Any`'s.
        let owner = if name == "sum" { "List" } else { "Any" };
        assert_eq!(
            owner_of(DispatchShape::List, sym, 0),
            owner,
            "{owner}.{name} should resolve for List"
        );
        assert_eq!(
            owner_of(DispatchShape::Array, sym, 0),
            owner,
            "{owner}.{name} should resolve for Array"
        );
        assert!(
            lookup(DispatchShape::Hash, sym, 0).is_some_and(|row| row.owner == "Any"),
            "Any.{name} should resolve for Hash"
        );
    }
    for name in ["permutations", "combinations"] {
        let sym = Symbol::intern(name);
        assert_eq!(owner_of(DispatchShape::List, sym, 0), "List");
        assert_eq!(owner_of(DispatchShape::Array, sym, 0), "List");
        // A `Hash` is `Any`'s `self.list.METHOD` (ADR-11276 §9.109).
        assert!(lookup(DispatchShape::Hash, sym, 0).is_some_and(|row| row.owner == "Any"));
    }
}

#[test]
fn hash_sum_rejects_its_pair_list() {
    // The receiver step reads the Hash as its pairs first (`Any.sum` is
    // `self.list.sum`), so the pure entry that runs it answers.
    let result = crate::builtins::methods_0arg::native_method_0arg(
        &sample(DispatchShape::Hash),
        Symbol::intern("sum"),
    );
    assert!(
        matches!(result, Some(Err(_))),
        "Hash.sum should reject Pairs"
    );
}
