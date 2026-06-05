//! Native `.sort` over a concrete array, for the no-comparator and
//! simple-comparator-block cases.
//!
//! `@a.sort` and `@a.sort({ $^a <=> $^b })` previously always fell back to the
//! interpreter's `dispatch_sort` (see docs/vm-decoupling.md, lever A). Those two
//! forms — by far the most common — need no user-code call at all: the
//! no-comparator sort uses the canonical `compare_values`, and a simple
//! `{ $^a <=> $^b }` / `{ $^b <=> $^a }` / `cmp` block is recognized by
//! `detect_simple_cmp_block` and compared inline. Running them in the VM keeps
//! them fully native (no perf regression — the interpreter already avoided the
//! block call here) and removes the `.sort` method fallback for these cases.
//!
//! Everything that genuinely needs the interpreter's richer orchestration —
//! general comparator/mapper blocks (`{ $^a.foo <=> $^b.foo }`, `*.abs`, a
//! Routine, …), the `:k`/`:kv`/`:p`/`:v` adverbs, and non-plain targets
//! (Shaped/Lazy/ItemArray arrays, Seq, Range, Hash) — falls back unchanged.

use super::*;
use crate::runtime::methods_collection_ops::sort::{detect_simple_cmp_block, inline_numeric_cmp};
use crate::runtime::utils::compare_values;
use crate::value::{ArrayKind, Value};

impl VM {
    /// Try to run `target.sort(...)` natively. Returns `Some(result)` when
    /// handled in the VM, `None` to fall back to the interpreter unchanged.
    ///
    /// `.sort` has no rw-view concern (it always yields a fresh `Seq`), so a
    /// freshly-built sorted array is a faithful result.
    pub(super) fn try_native_sort(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "sort" {
            return None;
        }
        // Only a plain, eager, single-level array. `List`/`Array` both sort into
        // a `Seq`; `Shaped` sorts over leaves, `Lazy` must stay lazy, and
        // item/scalar-wrapped arrays have their own semantics — all fall back.
        let items_arc = match target {
            Value::Array(items, ArrayKind::Array | ArrayKind::List) => items.clone(),
            _ => return None,
        };

        // Parse the (optional) comparator. Mirror `dispatch_sort`'s arg handling
        // for the subset we support: a single positional/`:by` callable, no
        // index-returning adverbs.
        let mut callable: Option<&Value> = None;
        for arg in args {
            match arg {
                Value::Pair(key, val) if key == "by" => callable = Some(val),
                // `:k`/`:kv`/`:p`/`:v` (return indices/pairs) and any other
                // adverb need the interpreter.
                Value::Pair(..) => return None,
                _ => callable = Some(arg),
            }
        }

        let mut items = items_arc.as_ref().clone();
        match callable {
            // `@a.sort` — canonical comparison.
            None => items.sort_by(|a, b| compare_values(a, b).cmp(&0)),
            // `@a.sort({ $^a <=> $^b })` and friends — inline comparison when the
            // block is a simple two-variable `<=>`/`cmp`. Any richer block (a
            // method-keyed comparator, a 1-arity mapper, …) returns `None` here
            // and falls back.
            Some(Value::Sub(data)) => {
                if data.empty_sig {
                    return None;
                }
                let (reverse, is_string_cmp) = detect_simple_cmp_block(data)?;
                items.sort_by(|a, b| {
                    let (l, r) = if reverse { (b, a) } else { (a, b) };
                    if is_string_cmp {
                        l.to_str_context().cmp(&r.to_str_context())
                    } else {
                        inline_numeric_cmp(l, r)
                    }
                });
            }
            // Routine / WhateverCode / anything else — fall back.
            Some(_) => return None,
        }

        Some(Ok(Value::Seq(std::sync::Arc::new(items))))
    }
}
