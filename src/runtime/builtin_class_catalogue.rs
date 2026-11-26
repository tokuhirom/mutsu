//! The static catalogue of built-in classes (ADR-19206).
//!
//! A built-in class whose definition is a constant of the binary is not built
//! at start-up: it is a row here, and its [`ClassDef`] is materialized the first
//! time anything looks it up, once per process, into a slot every interpreter
//! and thread shares. [`super::registry_class_table::ClassTable`] puts this
//! catalogue under the registry's copy-on-write class table; a write to a
//! built-in class copies it into that table first, so a slot is never written
//! after it is filled.
//!
//! Two kinds of row share one index space:
//!
//! - rows `0..X_LEN` are the ~370 `X::` exception classes that
//!   `build_builtin_registry` used to register one by one (`register_x`,
//!   slice 1), each described by its name, parent and composed roles;
//! - rows `X_LEN..LEN` are the attribute-less core classes whose `ClassDef`
//!   literal moved into `builtin_class_catalogue_core` (slice 2), each built
//!   by its own builder.
//!
//! The rows are `&'static str`s plus `const`-computed `u32` offset tables, so
//! the catalogue lives in `.rodata` and costs the dynamic loader no relocations
//! (ADR-19206 §2.2).

use std::sync::OnceLock;

use super::ClassDef;
use super::builtin_class_catalogue_core::{CORE_CLASS_ROWS, build_core_class};
use super::builtin_class_catalogue_x_rows::X_CLASS_ROWS;
use super::registry_cow_table::CowTable;
use crate::symbol::Symbol;

const X_ROWS: &[u8] = X_CLASS_ROWS.as_bytes();
const CORE_ROWS: &[u8] = CORE_CLASS_ROWS.as_bytes();

/// The number of `X::` rows; they come first.
const X_LEN: usize = count_rows(X_ROWS);
/// The number of core rows; they follow the `X::` rows.
const CORE_LEN: usize = count_rows(CORE_ROWS);
/// The number of catalogue rows.
pub(crate) const LEN: usize = X_LEN + CORE_LEN;

/// Byte offset of each `X::` row's first byte.
static ROW_START: [u32; X_LEN] = row_starts::<X_LEN>(X_ROWS);
/// Byte offset of the tab that ends each `X::` row's name.
static NAME_END: [u32; X_LEN] = name_ends::<X_LEN>(X_ROWS);
/// The same two tables for the core rows.
static CORE_ROW_START: [u32; CORE_LEN] = row_starts::<CORE_LEN>(CORE_ROWS);
static CORE_NAME_END: [u32; CORE_LEN] = name_ends::<CORE_LEN>(CORE_ROWS);

/// Each row's name as a `String`, built on the first whole-table walk (keys
/// of the class table are `String`s; start-up never walks it).
static NAME_STRINGS: OnceLock<Box<[String]>> = OnceLock::new();

/// Each row's materialized class, filled on first lookup.
static SLOTS: [OnceLock<CowTable<ClassDef>>; LEN] = [const { OnceLock::new() }; LEN];

/// The parents of the non-`X::`-row classes an `X::` row's MRO walk can reach
/// (core rows, whose `ClassDef` literals say so). Their own parents end the
/// walk at `Exception`, which has none. A unit test pins this table against
/// the built classes.
const EAGER_PARENTS: &[(&str, &str)] = &[
    ("X::AdHoc", "Exception"),
    ("X::TypeCheck", "Exception"),
    ("X::TypeCheck::Assignment", "X::TypeCheck"),
];

// ----- compile-time layout ------------------------------------------------

// Cost: O(n), n = bytes of the row data; compile time only.
const fn count_rows(rows: &[u8]) -> usize {
    let mut n = 0;
    let mut i = 0;
    while i < rows.len() {
        if rows[i] == b'\n' {
            n += 1;
        }
        i += 1;
    }
    assert!(
        !rows.is_empty() && rows[rows.len() - 1] == b'\n',
        "every catalogue row ends with a newline"
    );
    n
}

// Cost: O(n), n = bytes of the row data; compile time only.
const fn row_starts<const N: usize>(rows: &[u8]) -> [u32; N] {
    let mut out = [0u32; N];
    let mut row = 1;
    let mut i = 0;
    while i < rows.len() {
        if rows[i] == b'\n' && row < N {
            out[row] = (i + 1) as u32;
            row += 1;
        }
        i += 1;
    }
    out
}

// Cost: O(n), n = bytes of the row data; compile time only. Also checks that
// every row has three tab-separated fields and that the names are strictly
// increasing (the binary search below relies on it).
const fn name_ends<const N: usize>(rows: &[u8]) -> [u32; N] {
    let starts = row_starts::<N>(rows);
    let mut out = [0u32; N];
    let mut row = 0;
    while row < N {
        let start = starts[row] as usize;
        let mut i = start;
        while rows[i] != b'\t' {
            assert!(rows[i] != b'\n', "a catalogue row has no fields");
            i += 1;
        }
        assert!(i > start, "a catalogue row has an empty name");
        out[row] = i as u32;
        let mut j = i + 1;
        let mut tabs = 1;
        while rows[j] != b'\n' {
            if rows[j] == b'\t' {
                tabs += 1;
            }
            j += 1;
        }
        assert!(tabs == 2, "a catalogue row needs name, parent and does");
        if row > 0 {
            let prev_start = starts[row - 1] as usize;
            let prev_end = out[row - 1] as usize;
            assert!(
                bytes_lt(rows, prev_start, prev_end, start, i),
                "catalogue rows must be sorted by name, without duplicates"
            );
        }
        row += 1;
    }
    out
}

// Cost: O(len), compile time only.
const fn bytes_lt(s: &[u8], a0: usize, a1: usize, b0: usize, b1: usize) -> bool {
    let mut i = 0;
    loop {
        if a0 + i == a1 {
            return b0 + i < b1;
        }
        if b0 + i == b1 {
            return false;
        }
        if s[a0 + i] != s[b0 + i] {
            return s[a0 + i] < s[b0 + i];
        }
        i += 1;
    }
}

// ----- lookups ------------------------------------------------------------

/// The name of row `index`.
// Cost: O(1).
pub(crate) fn name(index: usize) -> &'static str {
    match index.checked_sub(X_LEN) {
        None => &X_CLASS_ROWS[ROW_START[index] as usize..NAME_END[index] as usize],
        Some(core) => {
            &CORE_CLASS_ROWS[CORE_ROW_START[core] as usize..CORE_NAME_END[core] as usize]
        }
    }
}

/// The name of row `index`, as the `String` a class-table key walk yields.
// Cost: O(1) after the first call, which allocates every row's name: O(LEN).
pub(crate) fn name_string(index: usize) -> &'static String {
    &NAME_STRINGS.get_or_init(|| (0..LEN).map(|row| name(row).to_string()).collect())[index]
}

/// The `parent` and `does` fields of row `index`; empty for a core row, whose
/// builder holds its parents.
// Cost: O(r), r = the row's length.
fn fields(index: usize) -> (&'static str, &'static str) {
    if index >= X_LEN {
        return ("", "");
    }
    let end = ROW_START
        .get(index + 1)
        .map_or(X_CLASS_ROWS.len(), |next| *next as usize)
        - 1;
    let rest = &X_CLASS_ROWS[NAME_END[index] as usize + 1..end];
    // `name_ends` checks every row's three fields at compile time.
    rest.split_once('\t').unwrap_or((rest, ""))
}

/// The real superclass of row `index`.
// Cost: O(r), r = the row's length.
pub(crate) fn parent(index: usize) -> &'static str {
    fields(index).0
}

/// The roles row `index` composes (ADR-0029), in declaration order.
// Cost: O(r), r = the row's length.
pub(crate) fn does(index: usize) -> impl Iterator<Item = &'static str> {
    fields(index).1.split(',').filter(|role| !role.is_empty())
}

/// The row of the built-in class `name`, if the catalogue has one.
// Cost: O(|name| log LEN).
pub(crate) fn index_of(name_: &str) -> Option<usize> {
    // Every `X::` row's name starts with `X::`; core rows are searched too.
    if name_.starts_with("X::")
        && let Some(index) = search(0, X_LEN, name_)
    {
        return Some(index);
    }
    search(X_LEN, LEN, name_)
}

/// Binary search for `name_` among rows `lo..hi`, which are sorted by name.
// Cost: O(|name| log (hi - lo)).
fn search(mut lo: usize, mut hi: usize, name_: &str) -> Option<usize> {
    while lo < hi {
        let mid = (lo + hi) / 2;
        match name(mid).cmp(name_) {
            std::cmp::Ordering::Less => lo = mid + 1,
            std::cmp::Ordering::Greater => hi = mid,
            std::cmp::Ordering::Equal => return Some(mid),
        }
    }
    None
}

/// The first parent of a class an `X::` row's MRO walk reaches: another `X::`
/// row, one of the classes in [`EAGER_PARENTS`], or none.
// Cost: O(|name| log LEN).
fn parent_of(name_: &str) -> Option<&'static str> {
    if let Some(index) = index_of(name_)
        && index < X_LEN
    {
        return Some(parent(index));
    }
    EAGER_PARENTS
        .iter()
        .find(|(class, _)| *class == name_)
        .map(|(_, parent)| *parent)
}

/// Row `index`'s class, built on its first lookup in this process.
// Cost: O(1) after the first call; the first builds the class in O(d log LEN),
// d = its MRO depth.
pub(crate) fn materialize(index: usize) -> &'static CowTable<ClassDef> {
    SLOTS[index].get_or_init(|| match index.checked_sub(X_LEN) {
        None => build(index).into(),
        Some(core) => build_core(core, name(index)).into(),
    })
}

/// Core row `core`'s class, as `build_builtin_registry` inserted it: its
/// builder's literal, and -- for a built-in exception class -- its rakudo
/// attributes marked for declaration on first access, as
/// `BuiltinClassTable::insert` marks them (#18654, #18956).
// Cost: O(s), s = the size of the class's literal sets.
fn build_core(core: usize, name_: &str) -> ClassDef {
    let mut class_def = build_core_class(core);
    if super::builtin_exception_attrs::may_declare(name_) {
        let class = class_def
            .mro
            .first()
            .copied()
            .filter(|own| own.as_str() == name_)
            .unwrap_or_else(|| Symbol::intern(name_));
        class_def.attributes.defer_builtin_exception(class);
    }
    class_def
}

/// The class row `index` describes, exactly as `register_x` used to build it:
/// one parent, the MRO from walking first parents (with `Exception` appended
/// if the walk did not reach it), and its rakudo attributes marked for
/// declaration on first access (#18654, #18956).
// Cost: O(d log LEN), d = the MRO depth.
fn build(index: usize) -> ClassDef {
    let own = Symbol::intern(name(index));
    let exception = Symbol::intern("Exception");
    let mut mro = vec![own];
    let mut cur = parent(index);
    loop {
        mro.push(Symbol::intern(cur));
        match parent_of(cur) {
            Some(next) if next != cur => cur = next,
            _ => break,
        }
    }
    if !mro.contains(&exception) {
        mro.push(exception);
    }
    let mut class_def = ClassDef {
        parents: vec![parent(index).to_string()],
        mro: mro.into(),
        ..ClassDef::default()
    };
    class_def.attributes.defer_builtin_exception(own);
    class_def
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_row_is_found_by_name() {
        for index in 0..LEN {
            assert_eq!(index_of(name(index)), Some(index), "{}", name(index));
        }
        assert_eq!(index_of("X::NoSuchClass::Here"), None);
        assert_eq!(index_of("NoSuchClass"), None);
        let exception = index_of("Exception").expect("core row");
        assert!(exception >= X_LEN);
        let adhoc = index_of("X::AdHoc").expect("core row");
        assert!(adhoc >= X_LEN, "X::AdHoc is a core row, not an `X::` row");
    }

    #[test]
    fn rows_parse() {
        let tlfr = index_of("X::TooLateForREPR").expect("row");
        assert_eq!(parent(tlfr), "X::Comp");
        assert_eq!(does(tlfr).collect::<Vec<_>>(), vec!["X::Comp"]);
        let adverb = index_of("X::Adverb").expect("row");
        assert_eq!(parent(adverb), "Exception");
        assert_eq!(does(adverb).count(), 0);
    }

    /// Every row builds the class `register_x` used to build: one parent, and
    /// the MRO from walking first parents through the class table (which
    /// pins [`EAGER_PARENTS`] against the eagerly built classes), with
    /// `Exception` appended when the walk missed it; and its attributes are
    /// marked for declaration on first access.
    #[test]
    fn every_row_builds_what_register_x_built() {
        let registry = crate::runtime::Interpreter::shared_builtin_registry();
        for index in 0..X_LEN {
            let class_def = build(index);
            let mut expected = vec![name(index).to_string()];
            let mut cur = parent(index).to_string();
            loop {
                expected.push(cur.clone());
                match registry
                    .classes
                    .get(&cur)
                    .and_then(|class| class.parents.first().cloned())
                {
                    Some(next) if next != cur => cur = next,
                    _ => break,
                }
            }
            if !expected.iter().any(|c| c == "Exception") {
                expected.push("Exception".to_string());
            }
            let mro: Vec<String> = class_def.mro.iter().map(|s| s.as_str().to_string()).collect();
            assert_eq!(mro, expected, "{}", name(index));
            assert_eq!(class_def.parents, vec![parent(index).to_string()]);
            assert!(class_def.attributes.is_deferred(), "{}", name(index));
            assert!(registry.classes.contains_key(name(index)));
        }
    }

    /// Every core row builds a class whose MRO starts with its own name (or
    /// is empty), and is visible in the built registry under that name.
    #[test]
    fn every_core_row_builds_and_is_visible() {
        let registry = crate::runtime::Interpreter::shared_builtin_registry();
        for index in X_LEN..LEN {
            let class_def = materialize(index);
            if let Some(first) = class_def.mro.first() {
                assert_eq!(first.as_str(), name(index));
            }
            assert!(registry.classes.contains_key(name(index)), "{}", name(index));
        }
    }

    #[test]
    fn mro_walks_rows_and_eager_parents() {
        let sig = materialize(index_of("X::Syntax::Signature").expect("row"));
        let mro: Vec<&str> = sig.mro.iter().map(|s| s.as_str()).collect();
        assert_eq!(
            mro,
            vec!["X::Syntax::Signature", "X::Syntax", "X::Comp", "Exception"]
        );
        assert_eq!(sig.parents, vec!["X::Syntax".to_string()]);
    }
}
