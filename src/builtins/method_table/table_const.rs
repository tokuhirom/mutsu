//! The method table's lookup index, evaluated at compile time (#12195).
//!
//! The table used to be built by the first method call of every process: it
//! interned every row's name, hashed every `(receiver, name, arity)` key and
//! walked every shape's MRO, so each slice of ADR-11276 added ~1.7M
//! instructions to any script that called a method. Everything here depends on
//! nothing but the rows and the type catalog, both `static` data, so it is a
//! `const` computation and the process pays nothing for it at run time.
//!
//! The pieces, all indexed without a [`crate::symbol::Symbol`] (symbols are
//! interned at run time; `table.rs` maps a symbol to its name index lazily, one
//! binary search per distinct name per thread):
//!
//! * [`ALL`]: every row once, in registration order. A row's index is its
//!   [`RowId`](super::RowId).
//! * [`NAMES`] / [`NAME_ROWS`]: the distinct method names, sorted, and the
//!   rows of each (in id order), for the lookups that have a name and an
//!   owner but no shape (`owner_row`).
//! * [`ARITIES`], [`SHAPES`], [`TYPE_SHAPES`]: per name, one bit per arity /
//!   per [`DispatchShape`] some registered row answers.
//! * [`ENTRY_KEYS`] / [`ENTRY_IDS`] / [`ENTRY_START`]: per name, the rows each
//!   `(receiver, arity)` resolves to, sorted by key.
//!
//! The registration rule is the one the run-time builder used and
//! `table::tests::const_index_matches_the_runtime_builder` still checks: walk
//! the shapes in order, each shape's MRO most-derived first, each reached
//! owner's rows in registration order, and let the first row for a
//! `(receiver, name, arity)` win.

#![allow(long_running_const_eval)]

use super::table::GROUPS;
use super::{MethodRow, RowFlags};
use crate::value::{DispatchShape, const_str_eq};

const fn count_rows() -> usize {
    let mut n = 0;
    let mut g = 0;
    while g < GROUPS.len() {
        let families = GROUPS[g];
        let mut f = 0;
        while f < families.len() {
            n += families[f].len();
            f += 1;
        }
        g += 1;
    }
    n
}

/// The number of rows.
pub(super) const N: usize = count_rows();

// A `RowId` is a `u16`, and the index tables below store `u16` positions with
// the top values reserved for the run-time memo's markers.
const _: () = assert!(N < 60_000, "rows no longer fit a u16 RowId");

const fn flatten() -> [&'static MethodRow; N] {
    let mut out = [&GROUPS[0][0][0]; N];
    let mut n = 0;
    let mut g = 0;
    while g < GROUPS.len() {
        let families: &'static [&'static [MethodRow]] = GROUPS[g];
        let mut f = 0;
        while f < families.len() {
            let rows: &'static [MethodRow] = families[f];
            let mut r = 0;
            while r < rows.len() {
                out[n] = &rows[r];
                n += 1;
                r += 1;
            }
            f += 1;
        }
        g += 1;
    }
    out
}

const ROWS: [&MethodRow; N] = flatten();

/// Every row once, indexed by [`RowId`](super::RowId).
pub(super) static ALL: [&MethodRow; N] = ROWS;

/// Bytewise order, shorter first on a common prefix: `-1`, `0` or `1`.
const fn cmp_str(a: &str, b: &str) -> i8 {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    let common = if a.len() < b.len() { a.len() } else { b.len() };
    let mut i = 0;
    while i < common {
        if a[i] != b[i] {
            return if a[i] < b[i] { -1 } else { 1 };
        }
        i += 1;
    }
    if a.len() == b.len() {
        0
    } else if a.len() < b.len() {
        -1
    } else {
        1
    }
}

/// Row `a` sorts before row `b` by `(name, id)`.
const fn name_less(a: u16, b: u16) -> bool {
    match cmp_str(ROWS[a as usize].name, ROWS[b as usize].name) {
        -1 => true,
        0 => a < b,
        _ => false,
    }
}

/// Row `a` sorts before row `b` by `(owner, id)`.
const fn owner_less(a: u16, b: u16) -> bool {
    match cmp_str(ROWS[a as usize].owner, ROWS[b as usize].owner) {
        -1 => true,
        0 => a < b,
        _ => false,
    }
}

const fn plain_less(a: u64, b: u64) -> bool {
    a < b
}

/// A stable bottom-up merge sort of `$arr[..$len]` by `$less`, usable in a
/// `const` (no closures, no `sort`).
macro_rules! const_sort {
    ($arr:ident, $len:expr, $less:ident) => {{
        let n = $len;
        let mut buf = $arr;
        let mut width = 1;
        while width < n {
            let mut lo = 0;
            while lo < n {
                let mid = if lo + width < n { lo + width } else { n };
                let hi = if lo + 2 * width < n {
                    lo + 2 * width
                } else {
                    n
                };
                let (mut i, mut j, mut k) = (lo, mid, lo);
                while i < mid && j < hi {
                    if $less($arr[j], $arr[i]) {
                        buf[k] = $arr[j];
                        j += 1;
                    } else {
                        buf[k] = $arr[i];
                        i += 1;
                    }
                    k += 1;
                }
                while i < mid {
                    buf[k] = $arr[i];
                    i += 1;
                    k += 1;
                }
                while j < hi {
                    buf[k] = $arr[j];
                    j += 1;
                    k += 1;
                }
                lo += 2 * width;
            }
            let mut t = 0;
            while t < n {
                $arr[t] = buf[t];
                t += 1;
            }
            width *= 2;
        }
    }};
}

const fn iota() -> [u16; N] {
    let mut out = [0u16; N];
    let mut i = 0;
    while i < N {
        out[i] = i as u16;
        i += 1;
    }
    out
}

/// Row ids sorted by `(name, id)`.
const NAME_ORDER: [u16; N] = {
    let mut order = iota();
    const_sort!(order, N, name_less);
    order
};

/// Row ids sorted by `(owner, id)`.
const OWNER_ORDER: [u16; N] = {
    let mut order = iota();
    const_sort!(order, N, owner_less);
    order
};

const fn count_names() -> usize {
    let mut n = 0;
    let mut i = 0;
    while i < N {
        if i == 0
            || cmp_str(
                ROWS[NAME_ORDER[i] as usize].name,
                ROWS[NAME_ORDER[i - 1] as usize].name,
            ) != 0
        {
            n += 1;
        }
        i += 1;
    }
    n
}

/// The number of distinct method names.
pub(super) const U: usize = count_names();

const _: () = assert!(U < 60_000, "names no longer fit the run-time memo");

struct NameIndex {
    names: [&'static str; U],
    /// `NAME_ROWS[start[u]..start[u + 1]]` are the rows named `names[u]`.
    start: [u16; U + 1],
    /// Each row's name index.
    row_name: [u16; N],
}

const fn build_names() -> NameIndex {
    let mut index = NameIndex {
        names: [""; U],
        start: [0; U + 1],
        row_name: [0; N],
    };
    let mut u = 0;
    let mut i = 0;
    while i < N {
        let row = ROWS[NAME_ORDER[i] as usize];
        if i == 0 || cmp_str(row.name, ROWS[NAME_ORDER[i - 1] as usize].name) != 0 {
            index.names[u] = row.name;
            index.start[u] = i as u16;
            u += 1;
        }
        index.row_name[NAME_ORDER[i] as usize] = (u - 1) as u16;
        i += 1;
    }
    index.start[U] = N as u16;
    index
}

const NAME_INDEX: NameIndex = build_names();

/// The distinct method names, sorted bytewise.
pub(super) static NAMES: [&str; U] = NAME_INDEX.names;
/// `NAME_ROWS[NAME_START[u]..NAME_START[u + 1]]` are the rows named `NAMES[u]`,
/// in id order.
pub(super) static NAME_START: [u16; U + 1] = NAME_INDEX.start;
pub(super) static NAME_ROWS: [u16; N] = NAME_ORDER;

const fn count_owners() -> usize {
    let mut n = 0;
    let mut i = 0;
    while i < N {
        if i == 0
            || cmp_str(
                ROWS[OWNER_ORDER[i] as usize].owner,
                ROWS[OWNER_ORDER[i - 1] as usize].owner,
            ) != 0
        {
            n += 1;
        }
        i += 1;
    }
    n
}

const P: usize = count_owners();

struct OwnerIndex {
    owners: [&'static str; P],
    /// `OWNER_ORDER[start[p]..start[p + 1]]` are the rows `owners[p]` declares.
    start: [u16; P + 1],
}

const fn build_owners() -> OwnerIndex {
    let mut index = OwnerIndex {
        owners: [""; P],
        start: [0; P + 1],
    };
    let mut p = 0;
    let mut i = 0;
    while i < N {
        let row = ROWS[OWNER_ORDER[i] as usize];
        if i == 0 || cmp_str(row.owner, ROWS[OWNER_ORDER[i - 1] as usize].owner) != 0 {
            index.owners[p] = row.owner;
            index.start[p] = i as u16;
            p += 1;
        }
        i += 1;
    }
    index.start[P] = N as u16;
    index
}

const OWNER_INDEX: OwnerIndex = build_owners();

/// The positions in `OWNER_ORDER` of the rows `owner` declares.
const fn owner_range(owner: &str) -> (usize, usize) {
    let mut p = 0;
    while p < P {
        if const_str_eq(OWNER_INDEX.owners[p], owner) {
            return (
                OWNER_INDEX.start[p] as usize,
                OWNER_INDEX.start[p + 1] as usize,
            );
        }
        p += 1;
    }
    (0, 0)
}

/// How many `(receiver, name, arity)` candidates `row` offers `shape`: one
/// per arity for the instance receiver, one more per arity for the type
/// object when the row answers one.
const fn candidates(row: &MethodRow, shape: DispatchShape) -> usize {
    let arities = (arity_hi(row) - row.arity + 1) as usize;
    (shape.has_instances() as usize + row.flags.contains(RowFlags::TYPE_OBJECT_OK) as usize)
        * arities
}

/// The largest arity the table registers `row` at: its own, or for a slurpy
/// row every one up to 7 (see [`MethodRow::arities`]).
const fn arity_hi(row: &MethodRow) -> u8 {
    if row.flags.contains(RowFlags::SLURPY) {
        7
    } else {
        row.arity
    }
}

/// The shapes with a catalog MRO, with the rows each shape reaches, in the
/// order the table registers them: calls `visit` ... written out as a macro
/// because a `const fn` takes no closure.
macro_rules! for_each_registration {
    (|$shape:ident, $row:ident, $id:ident| $body:block) => {{
        let mut s = 0;
        while s < DispatchShape::ALL.len() {
            let $shape = DispatchShape::ALL[s];
            if let Some(mro) =
                crate::builtin_types::catalog::builtin_type_mro_strs($shape.type_name())
            {
                let mut m = 0;
                while m < mro.len() {
                    if $shape.reaches_owner(mro[m]) || $shape.may_reach_audited_cool(mro[m]) {
                        let (lo, hi) = owner_range(mro[m]);
                        let mut k = lo;
                        while k < hi {
                            let $id = OWNER_ORDER[k];
                            let $row = ROWS[$id as usize];
                            // A `Mut` row is registered by its owner only (ADR-11276
                            // §9.23): no shape reaches it, so no pure entry, call-site
                            // lane or cross-check can run a handler that has effects.
                            if !$row.flags.contains(RowFlags::OWNER_ONLY)
                                && !$row.handler.is_mut()
                                && $shape.reaches(mro[m], $row.name)
                            {
                                $body
                            }
                            k += 1;
                        }
                    }
                    m += 1;
                }
            }
            s += 1;
        }
    }};
}

const fn count_candidates() -> usize {
    let mut n = 0;
    for_each_registration!(|shape, row, _id| {
        n += candidates(row, shape);
    });
    n
}

/// Every candidate registration, before the first-wins dedupe.
const G: usize = count_candidates();

struct Registered {
    /// Per name, the bit of each arity some registered row takes.
    arities: [u8; U],
    shapes: [u64; U],
    type_shapes: [u64; U],
    /// Entries after the first-wins dedupe, each `(receiver bits << 8 | arity)`
    /// with the row it resolves to, grouped by name and sorted by key.
    len: usize,
    keys: [u16; G],
    ids: [u16; G],
    /// `keys[start[u]..start[u + 1]]` are the entries of name `u`.
    start: [u16; U + 1],
}

const fn register() -> Registered {
    let mut out = Registered {
        arities: [0; U],
        shapes: [0; U],
        type_shapes: [0; U],
        len: 0,
        keys: [0; G],
        ids: [0; G],
        start: [0; U + 1],
    };
    // `(name << 16 | receiver << 8 | arity) << 32 | generation index`: sorting
    // these as integers groups by key and keeps generation order within one.
    let mut gen_key = [0u64; G];
    let mut gen_id = [0u16; G];
    let mut n = 0;
    for_each_registration!(|shape, row, id| {
        let name = NAME_INDEX.row_name[id as usize] as usize;
        let mut arity = row.arity;
        while arity <= arity_hi(row) {
            assert!(arity < 8, "an arity of 8 or more does not fit a mask byte");
            out.arities[name] |= 1 << arity;
            if shape.has_instances() {
                let recv = super::Receiver::instance(shape).to_bits() as u64;
                gen_key[n] = ((name as u64) << 16 | recv << 8 | arity as u64) << 32 | n as u64;
                gen_id[n] = id;
                n += 1;
            }
            if row.flags.contains(RowFlags::TYPE_OBJECT_OK) {
                let recv = super::Receiver::type_object(shape).to_bits() as u64;
                gen_key[n] = ((name as u64) << 16 | recv << 8 | arity as u64) << 32 | n as u64;
                gen_id[n] = id;
                n += 1;
            }
            arity += 1;
        }
        out.shapes[name] |= 1 << (shape as u64);
        if row.flags.contains(RowFlags::TYPE_OBJECT_OK) {
            out.type_shapes[name] |= 1 << (shape as u64);
        }
    });
    assert!(n == G);
    const_sort!(gen_key, G, plain_less);
    let mut i = 0;
    let mut name = 0;
    while i < G {
        let key = gen_key[i] >> 32;
        if i == 0 || key != gen_key[i - 1] >> 32 {
            let entry_name = (key >> 16) as usize;
            while name <= entry_name {
                out.start[name] = out.len as u16;
                name += 1;
            }
            out.keys[out.len] = (key & 0xffff) as u16;
            out.ids[out.len] = gen_id[(gen_key[i] & 0xffff_ffff) as usize];
            out.len += 1;
        }
        i += 1;
    }
    while name <= U {
        out.start[name] = out.len as u16;
        name += 1;
    }
    out
}

const REGISTERED: Registered = register();

/// Per name, one bit per arity some `Mut` row takes. Those rows are registered
/// by owner only, so they never set a bit of [`ARITIES`]; a mutating call tests
/// this mask before it looks anything up. A slurpy row sets every bit up to 7,
/// and bit 7 stands for every longer call too.
const fn mut_arities() -> [u8; U] {
    let mut out = [0u8; U];
    let mut id = 0;
    while id < N {
        let row = ROWS[id];
        if row.handler.is_mut() {
            let name = NAME_INDEX.row_name[id] as usize;
            let mut arity = row.arity;
            while arity <= arity_hi(row) {
                out[name] |= 1 << arity;
                arity += 1;
            }
        }
        id += 1;
    }
    out
}

/// The number of resolved `(receiver, name, arity)` entries.
const M: usize = REGISTERED.len;

const _: () = assert!(M < 60_000, "entries no longer fit a u16 offset");

const fn truncate<const L: usize>(src: &[u16; G]) -> [u16; L] {
    let mut out = [0u16; L];
    let mut i = 0;
    while i < L {
        out[i] = src[i];
        i += 1;
    }
    out
}

/// Per name, one bit per arity (0..8) some registered row takes.
pub(super) static ARITIES: [u8; U] = REGISTERED.arities;
/// The same bits for the `Mut` rows, which are not registered by shape.
pub(super) static MUT_ARITIES: [u8; U] = mut_arities();
/// Per name, one bit per [`DispatchShape`] some registered row answers on an instance.
pub(super) static SHAPES: [u64; U] = REGISTERED.shapes;
/// The same bits for a type object receiver.
pub(super) static TYPE_SHAPES: [u64; U] = REGISTERED.type_shapes;
/// `ENTRY_KEYS[ENTRY_START[u]..ENTRY_START[u + 1]]` are name `u`'s entries,
/// sorted: `receiver bits << 8 | arity`.
pub(super) static ENTRY_KEYS: [u16; M] = truncate::<M>(&REGISTERED.keys);
/// The row of each entry in [`ENTRY_KEYS`].
pub(super) static ENTRY_IDS: [u16; M] = truncate::<M>(&REGISTERED.ids);
pub(super) static ENTRY_START: [u16; U + 1] = REGISTERED.start;
