//! The composed roles of the built-in value types (`Int does Real`, `Array
//! does Positional`, the `Systemic` classes, ...), as static rows the
//! composed-role registries answer from on first lookup (ADR-19206, the rule
//! "nothing constant is built before it is used").
//!
//! `build_builtin_registry` used to write each of these as a `Vec` of
//! `String`s into `class_composed_roles` and `class_direct_composed_roles` at
//! every start. Most of these types are not catalogue classes (`Int` has no
//! `ClassDef`), and the flattened lists reach roles `role_parents` cannot see
//! (`Rational[Int,Int]`'s `Real`), so a row states both lists outright
//! instead of deriving one from the other.
//!
//! Each row is `name<TAB>direct<TAB>flattened`, the role lists separated by
//! spaces (a role name may contain a comma: `Rational[Int,Int]`), sorted by
//! name; the layout is checked at compile time as the class catalogue's is.
//! The lists were measured against raku v2026.07 (`T.^roles`); without them
//! `.^roles` answered an empty list for every one of these types.
//!
//! - `direct` is what `.^roles(:!transitive)` answers; empty means the row has
//!   no `class_direct_composed_roles` entry.
//! - `flattened` is the closure `class_composed_roles` holds.
//!
//! None of them has a `class_does_only_roles` entry.

use std::sync::OnceLock;

use super::builtin_class_catalogue::{count_rows, name_ends, row_starts};

const SEED_ROWS: &str = concat!(
    "Array\t\tPositional Iterable\n",
    "Bag\tBaggy\tBaggy QuantHash Associative\n",
    "Blob\t\tPositional[T] Stringy\n",
    "Buf\tBlob[T]\tBlob[T] Positional[T] Stringy\n",
    "CompUnit::Repository::FileSystem\t\tCompUnit::Repository\n",
    "Compiler\t\tSystemic\n",
    "Complex\t\tNumeric\n",
    "Distro\t\tSystemic\n",
    "Encoding::Decoder::Builtin\t\tEncoding::Decoder\n",
    "Encoding::Encoder::Builtin\t\tEncoding::Encoder\n",
    "FatRat\tRational[Int,Int]\tRational[Int,Int] Real Numeric\n",
    "Hash\t\tAssociative Iterable\n",
    "Int\tReal\tReal Numeric\n",
    "Kernel\t\tSystemic\n",
    "List\t\tPositional Iterable\n",
    "Map\t\tAssociative Iterable\n",
    "Mix\tMixy\tMixy Baggy QuantHash Associative\n",
    "Num\tReal\tReal Numeric\n",
    "Pair\t\tAssociative\n",
    "Raku\t\tSystemic\n",
    "Range\t\tPositional Iterable\n",
    "Rat\tRational[Int,Int]\tRational[Int,Int] Real Numeric\n",
    "Seq\tSequence Iterable\tSequence PositionalBindFailover Iterable\n",
    "Set\tSetty\tSetty QuantHash Associative\n",
    "Slip\t\tPositional Iterable\n",
    "Str\t\tStringy\n",
    "VM\t\tSystemic\n",
);

const ROWS: &[u8] = SEED_ROWS.as_bytes();

/// The number of seed rows.
pub(crate) const LEN: usize = count_rows(ROWS);

static ROW_START: [u32; LEN] = row_starts::<LEN>(ROWS);
static NAME_END: [u32; LEN] = name_ends::<LEN>(ROWS);

/// Each row's name as a `String`, built on the first whole-table walk.
static NAME_STRINGS: OnceLock<Box<[String]>> = OnceLock::new();

/// The name of row `index`.
// Cost: O(1).
pub(crate) fn name(index: usize) -> &'static str {
    &SEED_ROWS[ROW_START[index] as usize..NAME_END[index] as usize]
}

/// The name of row `index`, as the `String` a key walk yields.
// Cost: O(1) after the first call, which allocates every row's name: O(LEN).
pub(crate) fn name_string(index: usize) -> &'static String {
    &NAME_STRINGS.get_or_init(|| (0..LEN).map(|row| name(row).to_string()).collect())[index]
}

/// The `direct` and `flattened` fields of row `index`.
// Cost: O(r), r = the row's length.
fn fields(index: usize) -> (&'static str, &'static str) {
    let end = ROW_START
        .get(index + 1)
        .map_or(SEED_ROWS.len(), |next| *next as usize)
        - 1;
    let rest = &SEED_ROWS[NAME_END[index] as usize + 1..end];
    // `name_ends` checks every row's three fields at compile time.
    rest.split_once('\t').unwrap_or((rest, ""))
}

/// The roles row `index` composes directly; empty when it has no
/// `class_direct_composed_roles` entry.
// Cost: O(r), r = the row's length.
pub(crate) fn direct(index: usize) -> impl Iterator<Item = &'static str> {
    fields(index).0.split(' ').filter(|role| !role.is_empty())
}

/// The flattened set of roles row `index` composes.
// Cost: O(r), r = the row's length.
pub(crate) fn flattened(index: usize) -> impl Iterator<Item = &'static str> {
    fields(index).1.split(' ').filter(|role| !role.is_empty())
}

/// The row of `name_`, if it has one.
// Cost: O(|name| log LEN).
pub(crate) fn index_of(name_: &str) -> Option<usize> {
    let (mut lo, mut hi) = (0, LEN);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rows_parse_with_role_names_that_contain_commas() {
        let rat = index_of("Rat").expect("row");
        assert_eq!(direct(rat).collect::<Vec<_>>(), ["Rational[Int,Int]"]);
        assert_eq!(
            flattened(rat).collect::<Vec<_>>(),
            ["Rational[Int,Int]", "Real", "Numeric"]
        );
        let vm = index_of("VM").expect("row");
        assert_eq!(direct(vm).count(), 0);
        assert_eq!(flattened(vm).collect::<Vec<_>>(), ["Systemic"]);
        assert_eq!(index_of("Nope"), None);
        for row in 0..LEN {
            assert_eq!(index_of(name(row)), Some(row));
            assert!(flattened(row).next().is_some(), "{}", name(row));
        }
    }
}
