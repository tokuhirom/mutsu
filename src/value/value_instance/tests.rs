//! Concurrency pins for the instance attribute cell — above all
//! [`crate::value::InstanceAttrs::commit_attrs_delta`], the commit that made a mutable native
//! method safe to run on an instance another thread is also mutating
//! (tokuhirom/mutsu#7923).

use super::*;
use crate::symbol::Symbol;

fn attrs() -> InstanceAttrs {
    InstanceAttrs::new(Symbol::intern("T"), AttrMap::new(), 1, false)
}

/// The defect behind tokuhirom/mutsu#7923: a read-modify-write over the whole
/// attribute map discards whatever another thread committed in between.
/// `commit_attrs` loses it; `commit_attrs_delta` keeps it.
#[test]
fn delta_commit_keeps_a_concurrent_writers_key() {
    let cell = attrs();

    // Thread A reads the map...
    let working = cell.to_map();
    let before = working.bits_image();

    // ...thread B inserts `b` and commits while A is still working...
    cell.insert("b", Value::int(2));

    // ...and A commits its own insert of `a`.
    let mut updated = working;
    updated.insert("a", Value::int(1));
    cell.commit_attrs_delta(&before, &updated);

    assert_eq!(cell.as_map().get("a"), Some(&Value::int(1)));
    assert_eq!(
        cell.as_map().get("b"),
        Some(&Value::int(2)),
        "a key neither side touched must survive the commit"
    );

    // The whole-map replacement this replaced is what dropped `b`.
    let working = cell.to_map();
    cell.insert("c", Value::int(3));
    cell.commit_attrs(working);
    assert_eq!(cell.as_map().get("c"), None);
}

/// A key the method genuinely removed is removed, and nothing else is.
#[test]
fn delta_commit_applies_removals() {
    let cell = attrs();
    cell.insert("keep", Value::int(1));
    cell.insert("drop", Value::int(2));

    let working = cell.to_map();
    let before = working.bits_image();
    let mut updated = working;
    updated.remove("drop");
    // Another thread adds a key after the snapshot.
    cell.insert("late", Value::int(3));
    cell.commit_attrs_delta(&before, &updated);

    assert_eq!(cell.as_map().get("keep"), Some(&Value::int(1)));
    assert_eq!(cell.as_map().get("drop"), None);
    assert_eq!(cell.as_map().get("late"), Some(&Value::int(3)));
}

/// Both threads rewrote the same key: last commit wins, and the key the other
/// thread *added* still survives.
#[test]
fn delta_commit_last_writer_wins_on_a_contended_key() {
    let cell = attrs();
    cell.insert("x", Value::int(0));

    let working = cell.to_map();
    let before = working.bits_image();
    cell.insert("x", Value::int(9));
    cell.insert("other", Value::int(7));

    let mut updated = working;
    updated.insert("x", Value::int(1));
    cell.commit_attrs_delta(&before, &updated);

    assert_eq!(cell.as_map().get("x"), Some(&Value::int(1)));
    assert_eq!(cell.as_map().get("other"), Some(&Value::int(7)));
}

/// A handler that changed nothing must not write at all, so it cannot clobber
/// a concurrent writer even on a key it merely carried through.
#[test]
fn delta_commit_of_an_untouched_map_is_a_no_op() {
    let cell = attrs();
    cell.insert("x", Value::int(0));

    let working = cell.to_map();
    let before = working.bits_image();
    cell.insert("x", Value::int(5));
    cell.commit_attrs_delta(&before, &working);

    assert_eq!(cell.as_map().get("x"), Some(&Value::int(5)));
}

/// Two threads hammering one cell through the delta commit: every key
/// inserted must be present at the end.
#[test]
fn delta_commit_survives_real_concurrency() {
    let cell = Arc::new(attrs());
    let threads: Vec<_> = (0..4)
        .map(|t| {
            let cell = Arc::clone(&cell);
            std::thread::spawn(move || {
                for i in 0..200 {
                    let working = cell.to_map();
                    let before = working.bits_image();
                    let mut updated = working;
                    updated.insert(format!("t{t}_{i}"), Value::int(i));
                    cell.commit_attrs_delta(&before, &updated);
                }
            })
        })
        .collect();
    for t in threads {
        t.join().unwrap();
    }
    for t in 0..4 {
        for i in 0..200 {
            assert!(
                cell.contains_key(format!("t{t}_{i}").as_str()),
                "lost t{t}_{i}"
            );
        }
    }
}
