//! Tests for `fn_keys_index.rs`.

use super::*;

fn indexed(i: &Interpreter, base: &str) -> bool {
    i.dispatch
        .fn_keys_by_base
        .contains_key(&Symbol::intern(base))
}

/// The first miss after a clear is a single-name scan; the second builds
/// the complete index, after which an unregistered name is answered
/// without a scan and evicted names refill together.
#[test]
fn evicted_base_names_refill_in_one_pass() {
    let mut i = Interpreter::new();
    i.run("sub alpha() { 1 }\nsub beta() { 2 }\nsub gamma() { 3 }\n")
        .expect("setup program runs");
    i.invalidate_fn_resolution();

    // First miss: that name only, index still incomplete.
    assert!(!i.fn_keys_for_base("alpha").is_empty());
    assert!(!indexed(&i, "beta"));
    assert!(!i.dispatch.fn_keys_index.complete);

    // Second miss: the complete index, every base name at once.
    assert!(!i.fn_keys_for_base("beta").is_empty());
    assert!(i.dispatch.fn_keys_index.complete);
    assert!(indexed(&i, "gamma"));
    assert!(i.fn_keys_for_base("never-declared").is_empty());

    // Evicting two names marks both dirty; a plain base name is refilled
    // on its own from the qualified-name tail index (#9170), the other
    // one staying dirty until its own lookup.
    let gamma = i.fn_keys_for_base("gamma");
    i.invalidate_fn_resolution_for_keys([gamma[0]]);
    let alpha = i.fn_keys_for_base("alpha");
    i.invalidate_fn_resolution_for_keys([alpha[0]]);
    assert!(!indexed(&i, "alpha") && !indexed(&i, "gamma"));
    assert_eq!(&*i.fn_keys_for_base("alpha"), &*alpha);
    assert!(!indexed(&i, "gamma"), "only the looked-up name is refilled");
    assert_eq!(&*i.fn_keys_for_base("gamma"), &*gamma);
    assert!(i.dispatch.fn_keys_index.dirty.is_empty());

    // A dirty name the tail index cannot list (spelled with `/`) still
    // refills every dirty name in one pass.
    i.invalidate_fn_resolution_for_keys([alpha[0], gamma[0]]);
    i.evict_fn_keys_base(Symbol::intern("GLOBAL::infix:</>"));
    assert!(i.fn_keys_for_base("infix:</>").is_empty());
    assert!(
        indexed(&i, "alpha") && indexed(&i, "gamma"),
        "the pass refilled the other dirty names too"
    );
    assert!(i.dispatch.fn_keys_index.dirty.is_empty());
}

/// An unqualified registry key does not make every later dirty base name
/// refill by a pass (#9170): the index records unqualified keys per base
/// name and adds them to what the tail index lists.
#[test]
fn an_unqualified_key_is_refilled_without_a_pass() {
    let mut i = Interpreter::new();
    i.run("sub alpha() { 1 }\nsub beta() { 2 }\n")
        .expect("setup program runs");
    i.invalidate_fn_resolution();
    i.fn_keys_for_base("alpha");
    i.fn_keys_for_base("beta");
    assert!(i.dispatch.fn_keys_index.complete);

    // An unqualified key of `alpha`, announced as every write is.
    let def = {
        let alpha = i.fn_keys_for_base("alpha");
        i.routines().functions.get(&alpha[0]).cloned().unwrap()
    };
    let bare = Symbol::intern("alpha");
    i.routines_mut().functions_mut().insert(bare, def);
    i.invalidate_fn_resolution_for_keys([bare]);
    let beta = i.fn_keys_for_base("beta");
    i.invalidate_fn_resolution_for_keys([beta[0]]);

    // Both dirty names refill on their own: no pass ran, which would have
    // refilled the other one as well.
    let alpha = i.fn_keys_for_base("alpha");
    assert!(alpha.contains(&bare), "the unqualified key is listed");
    let sorted = |keys: &[Symbol]| {
        let mut v: Vec<&str> = keys.iter().map(|k| k.as_str()).collect();
        v.sort_unstable();
        v
    };
    assert_eq!(sorted(&alpha), sorted(&i.collect_fn_keys_for_base("alpha")));
    assert!(!indexed(&i, "beta"), "only the looked-up name is refilled");
    assert_eq!(&*i.fn_keys_for_base("beta"), &*beta);

    // A recorded key that is gone again is not listed.
    i.routines_mut().functions_mut().remove(&bare);
    i.invalidate_fn_resolution_for_keys([bare]);
    assert!(!i.fn_keys_for_base("alpha").contains(&bare));
}

/// The debug audit compares an index entry with a full scan only when the
/// functions map moved to a state it has not audited that base name under
/// ([#12119](https://github.com/tokuhirom/mutsu/issues/12119)); every other
/// call is a memo hit, however many routines are registered.
#[cfg(debug_assertions)]
#[test]
fn the_audit_scans_once_per_functions_map_state() {
    let mut i = Interpreter::new();
    i.run("sub alpha() { 1 }\nsub beta() { 2 }\n")
        .expect("setup program runs");

    assert!(i.fn_base_name_registered("alpha"));
    let first = i.dispatch.fn_keys_index.audit_scans;
    for _ in 0..100 {
        assert!(i.fn_base_name_registered("alpha"));
    }
    assert_eq!(
        i.dispatch.fn_keys_index.audit_scans, first,
        "an unchanged functions map and entry audit once, not per call"
    );

    // A base name nobody audited yet is audited once.
    assert!(i.fn_base_name_registered("beta"));
    assert!(i.fn_base_name_registered("beta"));
    assert_eq!(i.dispatch.fn_keys_index.audit_scans, first + 1);

    // A registration moves the map to a new state: the entry that
    // survived it (`alpha`'s is not evicted) is compared again, once.
    i.run("sub gamma() { 3 }\n").expect("second program runs");
    let before = i.dispatch.fn_keys_index.audit_scans;
    assert!(i.fn_base_name_registered("alpha"));
    assert!(i.fn_base_name_registered("alpha"));
    assert_eq!(i.dispatch.fn_keys_index.audit_scans, before + 1);
}

/// The audit keeps its teeth: a functions-map write that never announced
/// itself (no `invalidate_fn_resolution_for_keys`) leaves the index entry
/// stale, and the next gate call panics even though the entry was audited
/// under the map's previous state.
#[cfg(debug_assertions)]
#[test]
#[should_panic(expected = "stale fn_keys_by_base entry")]
fn a_functions_map_write_that_missed_its_invalidation_fails_the_audit() {
    let mut i = Interpreter::new();
    i.run("sub alpha() { 1 }\n").expect("setup program runs");
    assert!(i.fn_base_name_registered("alpha"));

    let existing = i.fn_keys_for_base("alpha")[0];
    let def = i
        .routines()
        .functions
        .get(&existing)
        .cloned()
        .expect("alpha is registered");
    let extra = Symbol::intern("GLOBAL::alpha/9");
    i.routines_mut().functions_mut().insert(extra, def);

    i.fn_base_name_registered("alpha");
}

/// A name registered after the index became complete is found: its
/// registration evicts (dirties) its base name, so the complete index
/// does not answer "no keys" for it.
#[test]
fn a_name_registered_after_completion_is_found() {
    let mut i = Interpreter::new();
    i.run("sub alpha() { 1 }\nsub beta() { 2 }\n")
        .expect("setup program runs");
    i.invalidate_fn_resolution();
    i.fn_keys_for_base("alpha");
    i.fn_keys_for_base("beta");
    assert!(i.dispatch.fn_keys_index.complete);

    i.run("sub delta() { 4 }\n").expect("second program runs");
    let fresh = i.collect_fn_keys_for_base("delta");
    assert!(!fresh.is_empty(), "delta is registered");
    assert_eq!(&*i.fn_keys_for_base("delta"), &*fresh);
}

/// A `&self` reader answers from a complete index without a functions-map
/// scan: no keys for a clean base name nobody registered, and the tail
/// index's keys for a dirty one (#17429: a block-declared `my sub` is
/// re-registered, so dirty, on every call of its enclosing routine, and each
/// `&name` read of it walked the whole map).
#[test]
fn a_complete_index_answers_a_shared_reader_without_a_scan() {
    let mut i = Interpreter::new();
    i.run("sub alpha() { 1 }\nsub beta() { 2 }\n")
        .expect("setup program runs");
    i.invalidate_fn_resolution();
    // Incomplete: only a scan can tell.
    assert!(i.peek_fn_keys_for_base("beta").is_none());
    i.fn_keys_for_base("alpha");
    i.fn_keys_for_base("beta");
    assert!(i.dispatch.fn_keys_index.complete);

    // Clean and never registered: no keys.
    Symbol::intern("not-a-routine");
    assert_eq!(i.peek_fn_keys_for_base("not-a-routine").as_deref(), Some(&[][..]));

    // Dirty: the same keys a scan finds, and still dirty afterwards (the
    // reader does no bookkeeping).
    let alpha = i.fn_keys_for_base("alpha");
    i.invalidate_fn_resolution_for_keys([alpha[0]]);
    assert!(i.dispatch.fn_keys_index.dirty.contains("alpha"));
    let peeked = i.peek_fn_keys_for_base("alpha").expect("answered from the tail index");
    assert_eq!(&*peeked, &*i.collect_fn_keys_for_base("alpha"));
    assert!(i.dispatch.fn_keys_index.dirty.contains("alpha"));
}
