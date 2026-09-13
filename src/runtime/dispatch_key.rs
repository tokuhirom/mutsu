//! Where a function dispatch key is spelled.
//!
//! A registered routine lives in `Registry::functions` under a `Symbol` whose
//! text encodes the package, the base name, and — for a `multi` — the arity and
//! the argument type signature:
//!
//! ```text
//! GLOBAL::plain                    a plain `sub`
//! GLOBAL::plain/2                  arity-keyed
//! GLOBAL::plain/2:Int,Str          arity + type signature
//! GLOBAL::plain/2#1f3a…            arity + the body fingerprint of one candidate
//! GLOBAL::plain/2__m3              the third `multi` candidate at that arity
//! ```
//!
//! Six call sites used to `format!` their own spelling of that grammar on every
//! dispatch, which is what made a call several times more expensive in mutsu
//! than in rakudo while mutsu's *arithmetic* was faster
//! ([#8300](https://github.com/tokuhirom/mutsu/issues/8300)). The builders here
//! are that grammar written once, and they do it without allocating: the text
//! goes into a reusable per-thread scratch buffer that is handed straight to
//! [`Symbol::lookup`] or [`Symbol::intern`] and never escapes.
//!
//! The scratch buffer is *taken* out of its thread-local for the duration of a
//! build rather than borrowed, so a nested build (one of these functions called
//! from inside another's callback) simply allocates its own buffer instead of
//! panicking on a double `RefCell` borrow.

use crate::symbol::Symbol;
use std::cell::RefCell;
use std::fmt::Write as _;

thread_local! {
    /// Scratch buffer for one key build. Grown to the longest key this thread
    /// has built and then reused; `const`-constructed, so it allocates nothing
    /// until the first build.
    static KEY_BUF: RefCell<String> = const { RefCell::new(String::new()) };
}

/// Build a key with `build` into the reusable scratch buffer and hand the
/// finished text to `finish`.
fn with_key<R>(build: impl FnOnce(&mut String), finish: impl FnOnce(&str) -> R) -> R {
    let mut buf = KEY_BUF.with(|b| std::mem::take(&mut *b.borrow_mut()));
    buf.clear();
    build(&mut buf);
    let out = finish(&buf);
    // Keep the larger of the two buffers: a nested build left its own here.
    KEY_BUF.with(|b| {
        let mut slot = b.borrow_mut();
        if slot.capacity() < buf.capacity() {
            *slot = buf;
        }
    });
    out
}

/// `"{pkg}::{name}"` — the plain package-qualified key.
fn write_qualified(buf: &mut String, pkg: &str, name: &str) {
    buf.push_str(pkg);
    buf.push_str("::");
    buf.push_str(name);
}

/// Look up `"{pkg}::{name}"` without interning it.
///
/// Every key that is actually registered was interned when it was registered,
/// so a `lookup` miss is a definitive "no such key" — and a probe for a
/// candidate that does not exist never grows the process-global symbol table.
pub(crate) fn qualified_lookup(pkg: &str, name: &str) -> Option<Symbol> {
    with_key(|b| write_qualified(b, pkg, name), Symbol::lookup)
}

/// Intern `"{pkg}::{name}"`. Use [`qualified_lookup`] for a probe; this is for
/// a key that is about to be *registered* under that spelling.
pub(crate) fn qualified_intern(pkg: &str, name: &str) -> Symbol {
    with_key(|b| write_qualified(b, pkg, name), Symbol::intern)
}

/// Run `f` with `"{pkg}::{name}"` built into the reusable scratch buffer.
///
/// For the tables that are still keyed by `String` rather than by [`Symbol`]
/// (`Registry::proto_subs`), where interning the probe would be pure waste.
pub(crate) fn with_qualified<R>(pkg: &str, name: &str, f: impl FnOnce(&str) -> R) -> R {
    with_key(|b| write_qualified(b, pkg, name), f)
}

/// Run `f` with the **code-variable** spelling `"&{name}"` built into the
/// reusable scratch buffer.
///
/// Not a registry key, but the same shape of waste: several probes per function
/// call ask whether a lexical `&name` callable shadows the package routine, and
/// each was `format!`ing the sigil onto the name to ask. One of them —
/// `exec_call_func_op_inner`'s `CALL-ME` override lookup — runs
/// unconditionally on **every** call.
pub(crate) fn with_amp_name<R>(name: &str, f: impl FnOnce(&str) -> R) -> R {
    with_key(
        |b| {
            b.push('&');
            b.push_str(name);
        },
        f,
    )
}

/// Look up `"{name}/{arity}"` (already package-qualified `name`).
pub(crate) fn arity_lookup(name: &str, arity: usize) -> Option<Symbol> {
    with_key(
        |b| {
            b.push_str(name);
            b.push('/');
            let _ = write!(b, "{arity}");
        },
        Symbol::lookup,
    )
}

/// Look up `"{name}/{arity}:{types joined by ,}"` (already package-qualified
/// `name`).
pub(crate) fn arity_types_lookup(name: &str, arity: usize, types: &[&str]) -> Option<Symbol> {
    with_key(
        |b| {
            b.push_str(name);
            b.push('/');
            let _ = write!(b, "{arity}");
            b.push(':');
            for (i, t) in types.iter().enumerate() {
                if i > 0 {
                    b.push(',');
                }
                b.push_str(t);
            }
        },
        Symbol::lookup,
    )
}

/// Look up `"{name}/{arity}#{fingerprint:x}"` (already package-qualified
/// `name`).
pub(crate) fn arity_fingerprint_lookup(
    name: &str,
    arity: usize,
    fingerprint: u64,
) -> Option<Symbol> {
    with_key(
        |b| {
            b.push_str(name);
            b.push('/');
            let _ = write!(b, "{arity}");
            b.push('#');
            let _ = write!(b, "{fingerprint:x}");
        },
        Symbol::lookup,
    )
}

/// Look up `"{pkg}::{name}/{arity}"`.
pub(crate) fn qualified_arity_lookup(pkg: &str, name: &str, arity: usize) -> Option<Symbol> {
    with_key(
        |b| {
            write_qualified(b, pkg, name);
            b.push('/');
            let _ = write!(b, "{arity}");
        },
        Symbol::lookup,
    )
}

/// Look up `"{pkg}::{name}/{arity}:{types joined by ,}"`.
pub(crate) fn qualified_arity_types_lookup(
    pkg: &str,
    name: &str,
    arity: usize,
    types: &[&str],
) -> Option<Symbol> {
    with_key(
        |b| {
            write_qualified(b, pkg, name);
            b.push('/');
            let _ = write!(b, "{arity}");
            b.push(':');
            for (i, t) in types.iter().enumerate() {
                if i > 0 {
                    b.push(',');
                }
                b.push_str(t);
            }
        },
        Symbol::lookup,
    )
}

/// Look up `"{pkg}::{name}/{arity}#{fingerprint:x}"`.
pub(crate) fn qualified_arity_fingerprint_lookup(
    pkg: &str,
    name: &str,
    arity: usize,
    fingerprint: u64,
) -> Option<Symbol> {
    with_key(
        |b| {
            write_qualified(b, pkg, name);
            b.push('/');
            let _ = write!(b, "{arity}");
            b.push('#');
            let _ = write!(b, "{fingerprint:x}");
        },
        Symbol::lookup,
    )
}

/// Whether `key` is one of `name`'s **arity-keyed candidate** spellings
/// declared in `pkg`: whether it starts with `"{pkg}::{name}/"`.
///
/// This is the predicate every `multi`-existence probe and candidate gather
/// applies, and it used to be written by `format!`ing that prefix into a fresh
/// `String` per package per call and calling `str::starts_with`, which builds a
/// two-way substring searcher. Comparing the three fixed-length pieces in place
/// allocates nothing and searches nothing.
pub(crate) fn key_is_candidate_of(key: &str, pkg: &str, name: &str) -> bool {
    let kb = key.as_bytes();
    let pb = pkg.as_bytes();
    let nb = name.as_bytes();
    let name_at = pb.len() + 2;
    let slash_at = name_at + nb.len();
    kb.len() > slash_at
        && kb[slash_at] == b'/'
        && kb[..pb.len()] == *pb
        && kb[pb.len()] == b':'
        && kb[pb.len() + 1] == b':'
        && kb[name_at..slash_at] == *nb
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn candidate_prefix_matches_the_formatted_spelling() {
        // The property the narrowed probes rely on: `key_is_candidate_of` is
        // exactly `key.starts_with(&format!("{pkg}::{name}/"))`.
        let cases = [
            ("GLOBAL::plain/2", "GLOBAL", "plain"),
            ("GLOBAL::plain/2:Int,Str", "GLOBAL", "plain"),
            ("GLOBAL::plain/2__m3", "GLOBAL", "plain"),
            ("GLOBAL::plain", "GLOBAL", "plain"),
            ("GLOBAL::plainer/2", "GLOBAL", "plain"),
            ("GLOBAL::plai/2", "GLOBAL", "plain"),
            ("M::plain/2", "GLOBAL", "plain"),
            ("GLOBAL::infix:</>/2", "GLOBAL", "infix:</>"),
            ("A::B::f/1", "A::B", "f"),
            ("A::Bx::f/1", "A::B", "f"),
            ("", "GLOBAL", "plain"),
            ("GLOBAL::plain/", "GLOBAL", "plain"),
        ];
        for (key, pkg, name) in cases {
            let expected = key.starts_with(&format!("{pkg}::{name}/"));
            assert_eq!(
                key_is_candidate_of(key, pkg, name),
                expected,
                "key_is_candidate_of({key:?}, {pkg:?}, {name:?})"
            );
        }
    }

    #[test]
    fn builders_spell_the_same_keys_format_did() {
        assert_eq!(
            qualified_intern("GLOBAL", "plain").as_str(),
            format!("{}::{}", "GLOBAL", "plain")
        );
        // The lookup forms find a key that exists and miss one that does not,
        // without interning the miss.
        let _ = Symbol::intern("GLOBAL::plain/2");
        let _ = Symbol::intern("GLOBAL::plain/2:Int,Str");
        let _ = Symbol::intern("GLOBAL::plain/2#1f3a");
        assert_eq!(
            qualified_arity_lookup("GLOBAL", "plain", 2).map(|s| s.as_str()),
            Some("GLOBAL::plain/2")
        );
        assert_eq!(
            qualified_arity_types_lookup("GLOBAL", "plain", 2, &["Int", "Str"]).map(|s| s.as_str()),
            Some("GLOBAL::plain/2:Int,Str")
        );
        assert_eq!(
            qualified_arity_fingerprint_lookup("GLOBAL", "plain", 2, 0x1f3a).map(|s| s.as_str()),
            Some("GLOBAL::plain/2#1f3a")
        );
        assert_eq!(
            arity_lookup("GLOBAL::plain", 2).map(|s| s.as_str()),
            Some("GLOBAL::plain/2")
        );
        assert_eq!(
            arity_types_lookup("GLOBAL::plain", 2, &["Int", "Str"]).map(|s| s.as_str()),
            Some("GLOBAL::plain/2:Int,Str")
        );
        assert_eq!(
            arity_fingerprint_lookup("GLOBAL::plain", 2, 0x1f3a).map(|s| s.as_str()),
            Some("GLOBAL::plain/2#1f3a")
        );
        assert_eq!(
            qualified_arity_lookup("GLOBAL", "no-such-routine-anywhere", 7),
            None
        );
    }

    #[test]
    fn the_scratch_buffer_survives_a_nested_build() {
        // `finish` builds another key; the outer text must still be intact.
        let inner = std::cell::Cell::new(None);
        let outer = with_key(
            |b| write_qualified(b, "Outer", "name"),
            |text| {
                inner.set(Some(qualified_intern("Inner", "name").as_str()));
                text.to_string()
            },
        );
        assert_eq!(outer, "Outer::name");
        assert_eq!(inner.get(), Some("Inner::name"));
        // And the buffer is usable again afterwards.
        assert_eq!(qualified_intern("After", "name").as_str(), "After::name");
    }
}
