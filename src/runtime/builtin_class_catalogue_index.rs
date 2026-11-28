//! The name index of the built-in class catalogue (ADR-19206): an
//! open-addressing hash table from a class name to its catalogue row, built
//! at compile time.
//!
//! [`super::builtin_class_catalogue::index_of`] answers every
//! `ClassTable::contains_key`/`get` on the registry's class table, hit or
//! miss, so it is on the path of almost every type-name question the
//! interpreter asks (`has_type_direct`, `is_builtin_type`, the package-chain
//! probes). Three binary searches over the sorted row sections cost ~25 string
//! comparisons per miss; a typed `my Tag $t` declaration inside a method asked
//! it ~6 times per execution (#12917). A hashed probe is one pass over the
//! name's bytes plus, at a load factor of at most 1/2, about one comparison.
//!
//! The table is a `static` of `u16`s computed by `const fn`, so like the rows
//! themselves it lives in `.rodata`: no start-up work and no relocations
//! (ADR-19206 §2.2).

use crate::static_str_set::fnv1a;

use super::builtin_class_catalogue::{
    ATTR_ROWS, ATTR_START, CORE_ROWS, LEN, X_LEN, X_ROWS, count_rows, name, name_ends, row_starts,
};

/// Slots in the table: at least twice the rows, so a probe sequence is short.
const SLOTS: usize = (2 * LEN).next_power_of_two();
const MASK: usize = SLOTS - 1;

/// Each slot holds `row + 1`, or `0` when empty.
static INDEX: [u16; SLOTS] = build();

const _: () = assert!(LEN < u16::MAX as usize, "catalogue rows must fit a u16 slot");

// Cost: O(n), n = bytes of all rows; compile time only.
const fn build() -> [u16; SLOTS] {
    let mut table = [0u16; SLOTS];
    insert_section::<{ count_rows(X_ROWS) }>(&mut table, X_ROWS, 0);
    insert_section::<{ count_rows(CORE_ROWS) }>(&mut table, CORE_ROWS, X_LEN);
    insert_section::<{ count_rows(ATTR_ROWS) }>(&mut table, ATTR_ROWS, ATTR_START);
    table
}

// Cost: O(n), n = bytes of the section's rows; compile time only.
const fn insert_section<const N: usize>(table: &mut [u16; SLOTS], rows: &[u8], first: usize) {
    let starts = row_starts::<N>(rows);
    let ends = name_ends::<N>(rows);
    let mut row = 0;
    while row < N {
        let mut slot = (fnv1a(rows, starts[row] as usize, ends[row] as usize) as usize) & MASK;
        while table[slot] != 0 {
            slot = (slot + 1) & MASK;
        }
        table[slot] = (first + row + 1) as u16;
        row += 1;
    }
}

/// The row of the built-in class `name_`, if the catalogue has one.
// Cost: O(|name|).
#[inline]
pub(super) fn lookup(name_: &str) -> Option<usize> {
    let bytes = name_.as_bytes();
    let mut slot = (fnv1a(bytes, 0, bytes.len()) as usize) & MASK;
    loop {
        let entry = INDEX[slot];
        if entry == 0 {
            return None;
        }
        let row = (entry - 1) as usize;
        if name(row) == name_ {
            return Some(row);
        }
        slot = (slot + 1) & MASK;
    }
}
