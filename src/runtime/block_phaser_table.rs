//! A Block's phaser table: rakudo's `nqp::getattr($block, Block, '$!phasers')`
//! and its `nqp::bindattr` (#17337).
//!
//! rakudo keeps a block's phasers in a hash attribute, phaser name -> list of
//! callables. Code that drives a block's iteration itself rewires that table on
//! a clone: ParaSeq's `BlockMapper` runs each batch of a hypered `map` on its
//! own clone of the user's block, giving the first batch the `FIRST` phasers
//! and the last batch the `LAST` ones, so each runs once across all batches.
//!
//! mutsu keeps a block's phasers as `Stmt::Phaser` statements in its body. So
//! reading the attribute builds the table from those statements; binding it on
//! a code object with a role mixed in (the shape ParaSeq binds on) stores the
//! table on the mixin; and a `.map` / `.grep` that iterates such a mixin
//! (`Interpreter::loop_callback`) runs a copy of the block whose loop phasers
//! (`FIRST`, `NEXT`, `LAST`) are the table's.

use super::*;
use crate::ast::PhaserKind;
use crate::value::SubData;
use std::hash::{Hash, Hasher};

/// The loop phasers a bound table replaces; the others (`ENTER`, `LEAVE`,
/// ...) run per call and stay the block's own.
const LOOP_PHASERS: [(&str, PhaserKind); 3] = [
    ("FIRST", PhaserKind::First),
    ("NEXT", PhaserKind::Next),
    ("LAST", PhaserKind::Last),
];

/// Every block phaser name the table lists.
const TABLE_PHASERS: [&str; 10] = [
    "FIRST", "NEXT", "LAST", "ENTER", "LEAVE", "KEEP", "UNDO", "PRE", "POST", "CLOSE",
];

/// The mixin attribute key a bound table is stored under. Spelled with `::`
/// and the `$!` twigil so no role attribute (stored by its bare name) can
/// collide with it.
fn table_key() -> Symbol {
    Symbol::intern("Block::$!phasers")
}

// Cost: O(b), b = size of the body.
fn body_hash(body: &[Stmt]) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    body.hash(&mut hasher);
    hasher.finish()
}

/// The callables a table entry holds: a list of them, or one callable.
// Cost: O(n), n = entries.
fn entry_callables(entry: &Value) -> Vec<Value> {
    // A list (`nqp::list`) or an `IterationBuffer` (`nqp::create(IB)`, as
    // ParaSeq builds them): the array behind it.
    if let Some(items) = crate::runtime::nqp_backing::with_nqp_backing_array(entry, |array| {
        match array.view() {
            ValueView::Array(items, _) => Some(items.iter().cloned().collect::<Vec<_>>()),
            _ => None,
        }
    }) {
        return items.unwrap_or_default();
    }
    match entry.view() {
        ValueView::Array(items, _) => items.iter().cloned().collect(),
        _ if entry.is_nil() => Vec::new(),
        _ => vec![entry.clone()],
    }
}

impl Interpreter {
    /// `nqp::getattr($code, Block, '$!phasers')`: the table bound on a mixin,
    /// or one built from the block's phaser statements; `None` when `obj` is
    /// not a block.
    // Cost: O(s), s = the block's top-level statements.
    pub(crate) fn block_phaser_table(obj: &Value) -> Option<Value> {
        if let ValueView::Mixin(_, mixins) = obj.view()
            && let Some(bound) = mixins.attributes().as_map().get(table_key())
        {
            return Some(bound.clone());
        }
        let inner = Self::unwrap_callable_mixin(obj.clone());
        let ValueView::Sub(data) = inner.view() else {
            return None;
        };
        let mut table = crate::value::ValueMap::default();
        for name in TABLE_PHASERS {
            let blocks = Self::block_phasers(&data, name);
            if !blocks.is_empty() {
                table.insert(name.to_string(), Value::array(blocks));
            }
        }
        Some(if table.is_empty() {
            Value::NIL
        } else {
            Value::vm_hash(table)
        })
    }

    /// `nqp::bindattr($code, Block, '$!phasers', $table)` on a code object
    /// with a role mixed in: the table is kept on the mixin. `false` when
    /// `obj` is not such a mixin.
    // Cost: O(1).
    pub(crate) fn bind_block_phaser_table(obj: &Value, table: Value) -> bool {
        let ValueView::Mixin(inner, mixins) = obj.view() else {
            return false;
        };
        if !matches!(inner.view(), ValueView::Sub(_)) {
            return false;
        }
        mixins.attributes().bind_attr_replace(table_key(), table);
        true
    }

    /// The block a `.map` / `.grep` iterates for the mixin `obj` over `data`:
    /// `data` itself, or, when a phaser table is bound on the mixin, a copy
    /// whose `FIRST` / `NEXT` / `LAST` phasers are the table's. A table entry
    /// that is one of the block's own phasers keeps that statement in place
    /// (it shares the block's lexical scope); any other entry runs as a call
    /// of the callable.
    // Cost: O(s + t), s = the block's top-level statements, t = table entries.
    pub(crate) fn block_with_bound_phasers(obj: &Value, data: &SubData) -> Option<Value> {
        let ValueView::Mixin(_, mixins) = obj.view() else {
            return None;
        };
        let table = mixins.attributes().as_map().get(table_key()).cloned()?;
        let entries: Vec<(PhaserKind, Vec<Value>)> = LOOP_PHASERS
            .iter()
            .map(|(name, kind)| {
                let callables = match table.view() {
                    ValueView::Hash(map) => map.get(*name).map(entry_callables),
                    _ => None,
                };
                (kind.clone(), callables.unwrap_or_default())
            })
            .collect();
        let mut pending: Vec<(PhaserKind, Value, Option<u64>)> = entries
            .into_iter()
            .flat_map(|(kind, callables)| {
                callables.into_iter().map(move |c| {
                    let own = match c.view() {
                        ValueView::Sub(sub) => Some(body_hash(&sub.body)),
                        _ => None,
                    };
                    (kind.clone(), c, own)
                })
            })
            .collect();
        let mut changed = false;
        let mut body: Vec<Stmt> = Vec::with_capacity(data.body.len() + pending.len());
        for stmt in data.body.iter() {
            if let Stmt::Phaser {
                kind,
                body: phaser_body,
                ..
            } = stmt
                && LOOP_PHASERS.iter().any(|(_, k)| k == kind)
            {
                let hash = body_hash(phaser_body);
                if let Some(at) = pending
                    .iter()
                    .position(|(k, _, own)| k == kind && *own == Some(hash))
                {
                    pending.remove(at);
                    body.push(stmt.clone());
                } else {
                    changed = true;
                }
                continue;
            }
            body.push(stmt.clone());
        }
        // The table is the block's own phasers: iterate the block itself, with
        // the code it was compiled to in its own scope.
        if !changed && pending.is_empty() {
            return None;
        }
        // Ahead of the body: a block's value is its last statement's, and a
        // phaser runs when its kind says, wherever it is written.
        //
        // Each callable is bound in the copy's closure env under a name no
        // Raku identifier can spell, and the phaser calls it by that name: the
        // compiled body is then the same for every batch (ParaSeq binds a
        // fresh wrapper closure per batch), and nothing hashes or embeds the
        // callable itself (a wrapper reaches the table it sits in).
        let mut added: Vec<Stmt> = Vec::with_capacity(pending.len());
        let mut bound: Vec<(String, Value)> = Vec::with_capacity(pending.len());
        for (n, (kind, callable, _)) in pending.into_iter().enumerate() {
            let name = format!("phaser table {n}");
            bound.push((name.clone(), callable));
            added.push(Stmt::Phaser {
                spans: None,
                kind,
                body: vec![Stmt::Expr(Expr::CallOn {
                    target: Box::new(Expr::Var(name)),
                    args: Vec::new(),
                })],
                condition: None,
                end_index: None,
                bare: false,
                scope: 0,
            });
        }
        added.extend(body);
        let body = added;
        let mut copy = data.clone();
        for (name, callable) in bound {
            copy.env.insert(name, callable);
        }
        let key = format!("phaser-table:{:x}", body_hash(&body));
        copy.body = std::sync::Arc::new(body);
        copy.compiled_routine = None;
        let (code, fns) =
            crate::runtime::synthesized_closure_code(&key, &copy.params, &copy.param_defs, &copy.body);
        copy.compiled_code = Some(code);
        copy.compiled_fns = Some(fns);
        Some(Value::sub_value(crate::gc::Gc::new(copy)))
    }
}
