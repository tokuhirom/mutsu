//! Tests for [`HashKey`](super::HashKey).
//!
//! Two of these are load-bearing rather than incidental:
//!
//! - `hash_agrees_with_str` / `borrowed_str_lookup_finds_the_key` pin the
//!   `Borrow<str>` contract. If `Hash` or `Eq` ever stopped agreeing with
//!   `str`'s, `HashMap<HashKey, _>::get("k")` would not fail loudly — it would
//!   silently report the key as missing, which in the interpreter reads as a
//!   hash that lost its contents.
//! - `hash_key_is_no_wider_than_string` pins the size. `HashKey` is the key of
//!   every hash table in the interpreter, so a variant that outgrows the
//!   `String` it replaces would cost more table memory than it saves in
//!   allocations.

use super::{HashKey, INLINE_CAP};
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

fn hash_of<T: Hash + ?Sized>(v: &T) -> u64 {
    let mut h = DefaultHasher::new();
    v.hash(&mut h);
    h.finish()
}

#[test]
fn hash_key_is_no_wider_than_string() {
    assert_eq!(
        std::mem::size_of::<HashKey>(),
        std::mem::size_of::<String>(),
        "HashKey must not be wider than the String it replaces"
    );
}

#[test]
fn short_keys_are_inline_long_keys_are_shared() {
    for len in 0..=INLINE_CAP {
        let s = "a".repeat(len);
        let k = HashKey::new(&s);
        assert!(k.is_inline(), "{len}-byte key should be inline");
        assert_eq!(k.as_str(), s);
    }
    for len in [INLINE_CAP + 1, INLINE_CAP + 2, 64, 4096] {
        let s = "a".repeat(len);
        let k = HashKey::new(&s);
        assert!(!k.is_inline(), "{len}-byte key should be shared");
        assert_eq!(k.as_str(), s);
    }
}

#[test]
fn round_trips_utf8_across_the_inline_boundary() {
    // The boundary is a BYTE boundary, so a multibyte char must never be
    // split: these strings straddle INLINE_CAP in bytes while staying valid.
    for s in [
        "",
        "a",
        "key-10000",
        "\u{3042}",                             // 3 bytes
        "\u{3042}\u{3044}\u{3046}",             // 9 bytes
        "\u{3042}\u{3044}\u{3046}ab",           // 11 bytes
        "abcdefghi\u{3042}\u{3044}",            // 15 bytes, exactly INLINE_CAP
        "abcdefghij\u{3042}\u{3044}",           // 16 bytes, one over
        "\u{1F600}\u{1F600}\u{1F600}",          // 12 bytes, 4-byte chars
        "\u{1F600}\u{1F600}\u{1F600}\u{1F600}", // 16 bytes
        "a very long key that will certainly not fit inline",
    ] {
        let k = HashKey::new(s);
        assert_eq!(k.as_str(), s, "round trip failed for {s:?}");
        assert_eq!(k.len(), s.len());
        assert_eq!(k.is_empty(), s.is_empty());
        assert_eq!(&*k, s, "Deref disagrees for {s:?}");
    }
}

#[test]
fn hash_agrees_with_str() {
    for s in [
        "",
        "a",
        "name",
        "test-depends",
        "abcdefghi\u{3042}\u{3044}",
        "Zef::Distribution::Local",
        "a very long key that will certainly not fit inline",
    ] {
        let k = HashKey::new(s);
        assert_eq!(
            hash_of(&k),
            hash_of(s),
            "HashKey and str hash differently for {s:?}"
        );
        // `String` delegates to `str`, so this is the same promise stated the
        // way the migration will actually exercise it.
        assert_eq!(hash_of(&k), hash_of(&s.to_string()));
    }
}

#[test]
fn borrowed_str_lookup_finds_the_key() {
    let mut m: HashMap<HashKey, i32> = HashMap::new();
    let keys = [
        "",
        "a",
        "name",
        "abcdefghi\u{3042}\u{3044}",
        "Zef::Distribution::Local",
        "a very long key that will certainly not fit inline",
    ];
    for (i, k) in keys.iter().enumerate() {
        m.insert(HashKey::new(k), i as i32);
    }
    for (i, k) in keys.iter().enumerate() {
        assert_eq!(
            m.get(*k),
            Some(&(i as i32)),
            "lookup by &str failed for {k:?}"
        );
        assert_eq!(m.get(&HashKey::new(k)), Some(&(i as i32)));
    }
    assert_eq!(m.get("absent"), None);
    assert_eq!(m.len(), keys.len());
}

#[test]
fn equal_keys_compare_equal_across_representations() {
    // The same text built two ways must be equal — a short key forced into the
    // shared representation via `Arc` still equals its inline twin, or a hash
    // built through different paths would grow duplicate entries.
    let inline = HashKey::new("name");
    let shared = HashKey::from(Arc::<str>::from("name"));
    assert!(inline.is_inline());
    assert!(!shared.is_inline());
    assert_eq!(inline, shared);
    assert_eq!(hash_of(&inline), hash_of(&shared));

    let mut m: HashMap<HashKey, i32> = HashMap::new();
    m.insert(inline, 1);
    m.insert(shared, 2);
    assert_eq!(m.len(), 1, "the two representations collapsed to one entry");
    assert_eq!(m.get("name"), Some(&2));
}

#[test]
fn ordering_agrees_with_str() {
    let mut keys: Vec<HashKey> = ["pear", "apple", "", "Banana", "apple pie", "\u{3042}"]
        .iter()
        .map(|s| HashKey::new(s))
        .collect();
    let mut strs: Vec<&str> = ["pear", "apple", "", "Banana", "apple pie", "\u{3042}"].to_vec();
    keys.sort();
    strs.sort();
    let sorted: Vec<&str> = keys.iter().map(|k| k.as_str()).collect();
    assert_eq!(sorted, strs);
}

#[test]
fn clone_shares_the_allocation_for_long_keys() {
    let long = "a very long key that will certainly not fit inline";
    let k = HashKey::new(long);
    let c = k.clone();
    assert_eq!(k, c);
    assert_eq!(c.as_str(), long);
    // Cloning must not copy the bytes — that is the whole point of the type.
    assert_eq!(
        k.as_str().as_ptr(),
        c.as_str().as_ptr(),
        "cloning a shared key copied its bytes"
    );

    // An inline key has no allocation to share, so its clone is a bitwise copy
    // with its own address; equality is all that is promised there.
    let s = HashKey::new("name");
    assert_eq!(s, s.clone());
}

#[test]
fn conversions_preserve_the_text() {
    let short = "name";
    let long = "a very long key that will certainly not fit inline";
    for s in [short, long] {
        assert_eq!(HashKey::from(s).as_str(), s);
        assert_eq!(HashKey::from(s.to_string()).as_str(), s);
        assert_eq!(HashKey::from(&s.to_string()).as_str(), s);
        assert_eq!(HashKey::from(s.to_string().into_boxed_str()).as_str(), s);
        assert_eq!(HashKey::from(Arc::<str>::from(s)).as_str(), s);
        assert_eq!(String::from(HashKey::from(s)), s);
    }
    // Owned conversions keep the inline/shared split by length, except
    // `From<Arc<str>>`, which deliberately keeps the allocation it was given.
    assert!(HashKey::from(short.to_string()).is_inline());
    assert!(!HashKey::from(long.to_string()).is_inline());
    assert!(!HashKey::from(Arc::<str>::from(short)).is_inline());
}

#[test]
fn cross_type_equality_works_in_both_directions() {
    let k = HashKey::new("name");
    assert_eq!(k, *"name");
    assert_eq!(k, "name");
    assert_eq!(k, "name".to_string());
    assert_eq!(*"name", k);
    assert_eq!("name", k);
    assert_eq!("name".to_string(), k);
    assert_ne!(k, "other");
    assert_ne!("other", k);
}

#[test]
fn default_is_empty_and_formats_like_a_string() {
    let d = HashKey::default();
    assert!(d.is_empty());
    assert_eq!(d.as_str(), "");
    assert!(d.is_inline());

    let k = HashKey::new("na\"me");
    assert_eq!(format!("{k}"), "na\"me");
    // Debug is quoted and escaped exactly as `str`'s, so a `HashData` dump
    // reads the same as it did with `String` keys.
    assert_eq!(format!("{k:?}"), format!("{:?}", "na\"me"));
}
