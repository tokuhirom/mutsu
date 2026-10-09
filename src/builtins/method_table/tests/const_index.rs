//! The compile-time index (`table_const`) against the run-time builder it
//! replaced (#12195): the builder is kept here, as the oracle, and every
//! lookup the table answers must agree with it.

use super::*;
use crate::symbol::Symbol;
use crate::value::DispatchShape;
use rustc_hash::FxHashMap;

struct Legacy {
    rows: FxHashMap<(Receiver, Symbol, u8), RowId>,
    owners: FxHashMap<(Symbol, Symbol, u8), RowId>,
    slurpy: FxHashMap<(Symbol, Symbol), RowId>,
    arities: FxHashMap<Symbol, u8>,
    mut_arities: FxHashMap<Symbol, u8>,
    shapes: FxHashMap<Symbol, u64>,
    type_shapes: FxHashMap<Symbol, u64>,
}

fn legacy() -> Legacy {
    let all: Vec<&'static MethodRow> = all_rows().collect();
    assert!(all.len() < usize::from(u16::MAX));
    let mut t = Legacy {
        rows: FxHashMap::default(),
        owners: FxHashMap::default(),
        slurpy: FxHashMap::default(),
        arities: FxHashMap::default(),
        mut_arities: FxHashMap::default(),
        shapes: FxHashMap::default(),
        type_shapes: FxHashMap::default(),
    };
    for (idx, row) in all.iter().enumerate() {
        let id = RowId::from_bits(idx as u16);
        let owner = Symbol::intern(row.owner);
        let name = Symbol::intern(row.name);
        for arity in row.arities() {
            t.owners.entry((owner, name, arity)).or_insert(id);
        }
        if row.flags.contains(RowFlags::SLURPY) {
            t.slurpy.entry((owner, name)).or_insert(id);
        }
        if row.handler.is_mut() {
            for arity in row.arities() {
                *t.mut_arities.entry(name).or_default() |= 1 << arity;
            }
        }
    }
    for shape in DispatchShape::ALL {
        let Some(mro) = crate::builtin_types::catalog::builtin_type_mro_syms(shape.type_name())
        else {
            continue;
        };
        for owner in mro.iter() {
            if !shape.reaches_owner(owner.as_str()) && !shape.may_reach_audited_cool(owner.as_str()) {
                continue;
            }
            for (idx, row) in all.iter().enumerate() {
                // A `Mut` row is registered by its owner only (ADR-11276 §9.23).
                if row.owner != owner.as_str()
                    || row.flags.contains(RowFlags::OWNER_ONLY)
                    || row.handler.is_mut()
                    || !shape.reaches(owner.as_str(), row.name)
                {
                    continue;
                }
                let id = RowId::from_bits(idx as u16);
                let name = Symbol::intern(row.name);
                for arity in row.arities() {
                    if shape.has_instances() {
                        t.rows
                            .entry((Receiver::instance(shape), name, arity))
                            .or_insert(id);
                    }
                    *t.arities.entry(name).or_default() |= 1 << arity;
                    if row.flags.contains(RowFlags::TYPE_OBJECT_OK) {
                        t.rows
                            .entry((Receiver::type_object(shape), name, arity))
                            .or_insert(id);
                    }
                }
                *t.shapes.entry(name).or_default() |= 1 << (shape as u64);
                if row.flags.contains(RowFlags::TYPE_OBJECT_OK) {
                    *t.type_shapes.entry(name).or_default() |= 1 << (shape as u64);
                }
            }
        }
    }
    t
}

#[test]
fn const_index_matches_the_runtime_builder() {
    let old = legacy();
    let mut names: Vec<&str> = all_rows().map(|row| row.name).collect();
    names.sort_unstable();
    names.dedup();
    let owners: std::collections::BTreeSet<&str> = all_rows().map(|row| row.owner).collect();
    assert!(names.len() > 100 && !old.rows.is_empty());
    let mut checked = 0usize;
    for name in names.iter().copied().chain(["no-such-method", "elems2"]) {
        let sym = Symbol::intern(name);
        let arities = old.arities.get(&sym).copied().unwrap_or(0);
        for arity in 0..10usize {
            let want = arity < 8 && arities & (1 << arity) != 0;
            assert_eq!(names_a_row(sym, arity), want, "{name}/{arity} names_a_row");
        }
        let mut_arities = old.mut_arities.get(&sym).copied().unwrap_or(0);
        for arity in 0..10usize {
            // A call longer than the masks go answers by bit 7.
            let want = mut_arities & (1 << arity.min(7)) != 0;
            assert_eq!(
                names_a_mut_row(sym, arity),
                want,
                "{name}/{arity} names_a_mut_row"
            );
        }
        for shape in DispatchShape::ALL {
            for type_object in [false, true] {
                let receiver = if type_object {
                    Receiver::type_object(shape)
                } else {
                    Receiver::instance(shape)
                };
                let masks = if type_object {
                    &old.type_shapes
                } else {
                    &old.shapes
                };
                let want = masks
                    .get(&sym)
                    .is_some_and(|m| m & (1 << (shape as u64)) != 0);
                assert_eq!(shape_has_row(receiver, sym), want, "{name} {shape:?} shape");
                for arity in 0..10usize {
                    let legacy_id = u8::try_from(arity).ok().and_then(|a| {
                        let named = arity < 8 && arities & (1 << arity) != 0;
                        let shaped = want;
                        (named && shaped)
                            .then(|| old.rows.get(&(receiver, sym, a)).copied())
                            .flatten()
                    });
                    let got = resolve(receiver, sym, arity);
                    assert_eq!(
                        got.map(RowId::to_bits),
                        legacy_id.map(RowId::to_bits),
                        "{name}/{arity} on {shape:?} (type object: {type_object})"
                    );
                    checked += 1;
                }
            }
        }
        for owner in &owners {
            let osym = Symbol::intern(owner);
            for arity in 0..300usize {
                let want = u8::try_from(arity)
                    .ok()
                    .and_then(|a| old.owners.get(&(osym, sym, a)).copied())
                    .or_else(|| {
                        let id = *old.slurpy.get(&(osym, sym))?;
                        (arity >= usize::from(row(id).arity)).then_some(id)
                    });
                assert_eq!(
                    owner_row(osym, sym, arity).map(RowId::to_bits),
                    want.map(RowId::to_bits),
                    "{owner}.{name}/{arity} owner_row"
                );
            }
        }
    }
    assert!(checked > 10_000);
}
