//! The lookup structure behind the built-in method table (ADR-11276 §2.6):
//! every group's rows, resolved once per process along each shape's MRO.

use super::{MethodRow, RowFlags};
use crate::symbol::Symbol;
use crate::value::{DispatchShape, Value};
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

use super::{collections, ctors_mop, instances, io_concurrency, mutating, scalars};

/// Every group's families. A group is one slice of ADR-11276 §10 and owns a
/// directory of its own, so a slice adds its rows to its group's
/// `FAMILIES` and never edits this list: the groups are listed here once, in
/// slice 3A.
static GROUPS: &[&[&[MethodRow]]] = &[
    scalars::FAMILIES,
    collections::FAMILIES,
    instances::FAMILIES,
    io_concurrency::FAMILIES,
    mutating::FAMILIES,
    ctors_mop::FAMILIES,
];

/// Every row of every group, in registration order.
pub(super) fn all_rows() -> impl Iterator<Item = &'static MethodRow> {
    GROUPS
        .iter()
        .flat_map(|families| families.iter())
        .flat_map(|rows| rows.iter())
}

/// What a call's receiver is to the table: an instance of a shape, or the type
/// object of a built-in type (`Int` in `Int.Bool`). A type object answers only
/// the rows flagged [`RowFlags::TYPE_OBJECT_OK`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Receiver {
    pub(crate) shape: DispatchShape,
    pub(crate) type_object: bool,
}

impl Receiver {
    /// The receiver of an instance of `shape`.
    // Cost: O(1).
    pub(crate) const fn instance(shape: DispatchShape) -> Receiver {
        Receiver {
            shape,
            type_object: false,
        }
    }

    /// The receiver of the type object of `shape`'s type.
    // Cost: O(1).
    pub(crate) const fn type_object(shape: DispatchShape) -> Receiver {
        Receiver {
            shape,
            type_object: true,
        }
    }

    /// What `target` is to the table, or `None` when the table does not
    /// answer calls on it: it is neither a plain instance of a shape nor the
    /// type object of one.
    // Cost: O(1), a tag probe; a `Package` also looks its name up.
    #[inline]
    pub(crate) fn of(target: &Value) -> Option<Receiver> {
        match target.dispatch_shape() {
            Some(shape) => Some(Receiver::instance(shape)),
            None => target.type_object_shape().map(Receiver::type_object),
        }
    }

    /// The receiver packed into a byte: the shape and a type-object bit.
    // Cost: O(1).
    pub(crate) const fn to_bits(self) -> u8 {
        self.shape as u8 | ((self.type_object as u8) << 7)
    }
}

/// `(receiver, method, arity) -> row`, resolved along each shape's MRO, plus
/// the arities each method name has rows for.
pub(super) struct Table {
    pub(super) rows: FxHashMap<(Receiver, Symbol, u8), RowId>,
    /// `(owner, method, arity) -> row`, for a receiver the table has no shape
    /// for (an instance of a user subclass) whose MRO names the owner.
    owners: FxHashMap<(Symbol, Symbol, u8), RowId>,
    /// `(owner, method) -> row` for a slurpy row, which answers any arity from
    /// its own up.
    slurpy: FxHashMap<(Symbol, Symbol), RowId>,
    /// Every row once, indexed by [`RowId`].
    pub(super) all: Vec<&'static MethodRow>,
    /// Per `Symbol` id, one bit per arity some row with that name takes (bit
    /// `a` for `a` arguments). Most calls are to methods with no row, or with
    /// a row for another arity (`$s.index($n, $from)` beside the one-needle
    /// row); testing a bit answers those without hashing.
    arities: Vec<u8>,
    /// Per `Symbol` id, one bit per [`DispatchShape`] some row with that name
    /// is resolved for on an instance. A name with rows for other receivers
    /// (`Int` has rows on the numeric types, none on `Str`) is refused for
    /// this one by a bit test too, without the hash lookup or a call site's
    /// memo.
    shapes: Vec<u64>,
    /// The same bits for a type object receiver: only rows flagged
    /// `TYPE_OBJECT_OK` set them.
    type_shapes: Vec<u64>,
}

impl Table {
    /// Whether some row with `method`'s name is resolved for `receiver`.
    // Cost: O(1), a bit test.
    #[inline]
    pub(super) fn has_shape(&self, method: Symbol, receiver: Receiver) -> bool {
        let bits = if receiver.type_object {
            &self.type_shapes
        } else {
            &self.shapes
        };
        bits.get(method.id() as usize)
            .is_some_and(|bits| bits & (1 << (receiver.shape as u64)) != 0)
    }

    /// Whether some row is named `method` and takes `arity` arguments.
    // Cost: O(1), a bit test.
    #[inline]
    pub(super) fn has_name(&self, method: Symbol, arity: usize) -> bool {
        arity < 8
            && self
                .arities
                .get(method.id() as usize)
                .is_some_and(|bits| bits & (1 << arity) != 0)
    }
}

/// A row's index in the table: what a call site's inline cache remembers
/// (`vm_method_site_lane`). Stable for the life of the process.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct RowId(u16);

impl RowId {
    /// The id as the `u16` a site cache word packs.
    pub(crate) fn to_bits(self) -> u16 {
        self.0
    }

    /// The id a site cache word packed with [`Self::to_bits`]. Only ever
    /// given bits that came from `to_bits` in this process.
    pub(crate) fn from_bits(bits: u16) -> Self {
        Self(bits)
    }
}

/// Built on the first lookup, not at `Interpreter` construction: the cost is
/// one pass over the rows per shape (a few hundred inserts today).
pub(super) fn table() -> &'static Table {
    static TABLE: OnceLock<Table> = OnceLock::new();
    TABLE.get_or_init(build)
}

fn build() -> Table {
    let all: Vec<&'static MethodRow> = all_rows().collect();
    let mut table = Table {
        rows: FxHashMap::default(),
        owners: FxHashMap::default(),
        slurpy: FxHashMap::default(),
        all,
        arities: Vec::new(),
        shapes: Vec::new(),
        type_shapes: Vec::new(),
    };
    // Each row's name, interned once, and the rows of each owner in
    // registration order: the shape pass below visits an owner per MRO entry
    // per shape, so it must not rescan every row to find them.
    let mut names: Vec<Symbol> = Vec::with_capacity(table.all.len());
    let mut by_owner: FxHashMap<&'static str, Vec<usize>> = FxHashMap::default();
    // Rows of one owner sit together, so the owner symbol is interned once
    // per run of rows rather than once per row.
    let mut last_owner: Option<(&'static str, Symbol)> = None;
    for (idx, row) in table.all.iter().enumerate() {
        let name = Symbol::intern(row.name);
        names.push(name);
        by_owner.entry(row.owner).or_default().push(idx);
        // A row past `u16::MAX` stays unreachable through the table.
        if let Ok(id) = u16::try_from(idx) {
            let owner = match last_owner {
                Some((text, sym)) if text == row.owner => sym,
                _ => {
                    let sym = Symbol::intern(row.owner);
                    last_owner = Some((row.owner, sym));
                    sym
                }
            };
            for arity in row.arities() {
                table
                    .owners
                    .entry((owner, name, arity))
                    .or_insert(RowId(id));
            }
            if row.flags.contains(RowFlags::SLURPY) {
                table.slurpy.entry((owner, name)).or_insert(RowId(id));
            }
        }
    }
    for shape in DispatchShape::ALL {
        let Some(mro) = crate::builtin_types::catalog::builtin_type_mro_syms(shape.type_name())
        else {
            continue;
        };
        for owner in mro.iter() {
            // A closed shape (`DispatchShape::inherits`) reaches only the rows
            // its own type and the ancestors it names own.
            if !shape.reaches(owner.as_str()) {
                continue;
            }
            let Some(idxs) = by_owner.get(owner.as_str()) else {
                continue;
            };
            for &idx in idxs {
                let row = table.all[idx];
                // A row past `u16::MAX` stays unreachable through the table.
                if !row.flags.contains(RowFlags::OWNER_ONLY)
                    && let Ok(id) = u16::try_from(idx)
                {
                    table.register(shape, row, names[idx], RowId(id));
                }
            }
        }
    }
    table
}

impl Table {
    /// Make `row` (at `id`) the answer for `shape`, unless a more derived row
    /// already is.
    fn register(&mut self, shape: DispatchShape, row: &MethodRow, name: Symbol, id: RowId) {
        let slot = name.id() as usize;
        for arity in row.arities() {
            if shape.has_instances() {
                self.rows
                    .entry((Receiver::instance(shape), name, arity))
                    .or_insert(id);
            }
            if self.arities.len() <= slot {
                self.arities.resize(slot + 1, 0);
            }
            self.arities[slot] |= 1 << arity;
            if row.flags.contains(RowFlags::TYPE_OBJECT_OK) {
                self.rows
                    .entry((Receiver::type_object(shape), name, arity))
                    .or_insert(id);
            }
        }
        set_bit(&mut self.shapes, slot, shape);
        if row.flags.contains(RowFlags::TYPE_OBJECT_OK) {
            set_bit(&mut self.type_shapes, slot, shape);
        }
    }
}

/// Set `shape`'s bit in the mask of the symbol at `slot`.
fn set_bit(masks: &mut Vec<u64>, slot: usize, shape: DispatchShape) {
    if masks.len() <= slot {
        masks.resize(slot + 1, 0);
    }
    masks[slot] |= 1 << (shape as u64);
}

/// Whether any row is named `method` and takes `arity` arguments: the test
/// every lookup makes first.
// Cost: O(1), a bit test.
#[inline]
pub(crate) fn names_a_row(method: Symbol, arity: usize) -> bool {
    table().has_name(method, arity)
}

/// Whether a receiver of `receiver` may have a row for `method`: the second
/// bit test, once the receiver is known. `false` is definite.
// Cost: O(1), a bit test.
#[inline]
pub(crate) fn shape_has_row(receiver: Receiver, method: Symbol) -> bool {
    table().has_shape(method, receiver)
}

/// The row a receiver dispatches `method` to when called with `arity`
/// positional arguments, if the table has one.
// Cost: O(1), a bit test and one hash lookup.
#[inline]
pub(crate) fn resolve(receiver: Receiver, method: Symbol, arity: usize) -> Option<RowId> {
    let table = table();
    if !table.has_name(method, arity) || !table.has_shape(method, receiver) {
        return None;
    }
    let arity = u8::try_from(arity).ok()?;
    table.rows.get(&(receiver, method, arity)).copied()
}

/// The row `owner` declares for `method` taking `arity` positional arguments,
/// whatever the receiver is: the lookup of a receiver that has no shape. A
/// slurpy row answers every arity from its own up, however long.
// Cost: O(1), two hash lookups at most.
pub(crate) fn owner_row(owner: Symbol, method: Symbol, arity: usize) -> Option<RowId> {
    let table = table();
    if let Some(&id) = u8::try_from(arity)
        .ok()
        .and_then(|arity| table.owners.get(&(owner, method, arity)))
    {
        return Some(id);
    }
    let id = *table.slurpy.get(&(owner, method))?;
    (arity >= usize::from(row(id).arity)).then_some(id)
}

/// The row `id` names.
// Cost: O(1).
#[inline]
pub(crate) fn row(id: RowId) -> &'static MethodRow {
    table().all[usize::from(id.0)]
}

/// The row a receiver dispatches `method` to, if any (the lookup
/// [`super::try_dispatch`] makes, for tests).
#[cfg(test)]
pub(super) fn lookup(receiver: Receiver, method: Symbol, arity: u8) -> Option<&'static MethodRow> {
    let table = table();
    if !table.has_name(method, usize::from(arity)) {
        return None;
    }
    table
        .rows
        .get(&(receiver, method, arity))
        .map(|id| row(*id))
}
