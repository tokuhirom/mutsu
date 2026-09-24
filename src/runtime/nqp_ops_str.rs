//! The `nqp::` string and hash primitives.
//!
//! Third link in the pure-value chain (`nqp_ops` → `nqp_ops_process` →
//! `nqp_ops_text` → here), split for the 500-line limit.
//!
//! Every string op is **grapheme-indexed**, as MoarVM's are, and is the
//! same routine as the matching `Str` method (`builtins::str_prim`,
//! ADR-0117): nqp's `substr`, `index` and `chars` all count the same units,
//! and nqp code mixes them freely
//! (`nqp::substr($s, 0, nqp::index($s, $needle))`).

use super::*;

fn sarg(args: &[Value], i: usize) -> String {
    args.get(i).map(|v| v.to_string_value()).unwrap_or_default()
}

/// A string operand as a `Str` value: a `Str` argument is shared (its
/// payload may be referenced as a strand, ADR-0120), anything else is
/// stringified.
fn str_operand(args: &[Value], i: usize) -> Value {
    match args.get(i) {
        Some(v) if v.is_str_value() => v.clone(),
        _ => Value::str(sarg(args, i)),
    }
}

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

impl Interpreter {
    /// Try a string / hash `nqp::` op. `None` means "not an op this table
    /// knows"; the caller then raises the unsupported-op error.
    pub(crate) fn call_nqp_op_str(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        use crate::builtins::str_prim::{self, Fold};
        Some(match op {
            // Every string op below is the SAME routine the matching `Str`
            // method uses (`builtins::str_prim`, ADR-0117): positions are
            // graphemes, as in MoarVM, never codepoints.
            // nqp::substr($s, $from, $want?).
            // Cost: O(k) amortized, k = graphemes returned.
            "substr" => str_prim::nqp_substr(
                args.first().unwrap_or(&Value::NIL),
                iarg(args, 1),
                (args.len() > 2).then(|| iarg(args, 2)),
            ),
            // Cost: see str_prim::concat (O(1) as strands, O(n1 + n2) flat).
            "concat" => Ok(str_prim::concat(
                str_operand(args, 0),
                args.get(1).unwrap_or(&Value::NIL),
            )),
            // nqp::index / rindex return **-1** when the needle is absent,
            // where Raku's `index` returns Nil. nqp code branches on exactly
            // that, so the -1 is the contract, not a placeholder.
            // Cost: O((n - from) * m), n = graphemes of haystack, m = chars of needle.
            "index" | "indexic" | "indexim" | "indexicim" => {
                let fold = match op {
                    "indexic" => Fold::Case,
                    "indexim" => Fold::Mark,
                    "indexicim" => Fold::CaseMark,
                    _ => Fold::Exact,
                };
                Ok(Value::int(str_prim::nqp_index(
                    args.first().unwrap_or(&Value::NIL),
                    &sarg(args, 1),
                    iarg(args, 2),
                    fold,
                )))
            }
            // Cost: O((from - p) * m), p = the hit, m = chars of needle.
            "rindex" => str_prim::nqp_rindex(
                args.first().unwrap_or(&Value::NIL),
                &sarg(args, 1),
                (args.len() > 2).then(|| iarg(args, 2)),
            )
            .map(Value::int),
            // Cost: O(n), n = chars of $s.
            "flip" => Ok(str_prim::flip(&sarg(args, 0))),
            // Cost: O(n), n = chars of $s.
            "uc" => Ok(Value::str(crate::builtins::unicode::grapheme_uppercase(
                &sarg(args, 0),
            ))),
            // Cost: O(n), n = chars of $s.
            "lc" => Ok(Value::str(crate::builtins::unicode::grapheme_lowercase(
                &sarg(args, 0),
            ))),
            // Cost: O(n), n = chars of $s (one repeat strand, ADR-0120; see
            // str_prim::repeat).
            "x" => {
                let n = iarg(args, 1);
                if n < 0 {
                    return Some(Err(RuntimeError::new(format!(
                        "Repeat count ({n}) cannot be negative"
                    ))));
                }
                str_prim::repeat(&str_operand(args, 0), n as usize)
            }

            // -- hash primitives --
            // These mutate the hash IN PLACE, through the shared `Gc`, because
            // that is what nqp callers assume: `abbrev` reaches for a Map's
            // storage once and then builds it with bindkey/deletekey, expecting
            // the Map it already holds to reflect every write.
            // Cost: O(m) average, m = chars of the key.
            "bindkey" | "deletekey" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let target = crate::runtime::types::unwrap_varref_value(target);
                let key = sarg(args, 1);
                let ValueView::Hash(hash) = target.view() else {
                    return Some(Err(RuntimeError::new(format!(
                        "nqp::{op}: expected a hash"
                    ))));
                };
                let val = args.get(2).cloned().unwrap_or(Value::NIL);
                // SAFETY: audited aliased in-place container write (see
                // value::gc_contents_mut and docs/gc-contents-mut-inventory.md)
                // — no borrow into the hash is live across the write.
                let data = unsafe { crate::value::gc_contents_mut(&hash) };
                if op == "bindkey" {
                    data.map.insert(key, val.clone());
                    Ok(val)
                } else {
                    Ok(data.map.remove(&key).unwrap_or(Value::NIL))
                }
            }
            // Cost: O(m) average, m = chars of the key.
            "existskey" => {
                let target = crate::runtime::types::unwrap_varref_value(
                    args.first().cloned().unwrap_or(Value::NIL),
                );
                let key = sarg(args, 1);
                let yes = match target.view() {
                    ValueView::Hash(map) => map.get(&key).is_some(),
                    _ => false,
                };
                Ok(Value::int(yes as i64))
            }

            // nqp::clone($x) — a shallow copy that does not share the
            // original's container, so mutating the copy leaves the original
            // alone (nqp's `clone`, not Raku's `.clone` method dispatch).
            // `clone_nd` is the no-decontainerize sibling; operands are
            // already decontainerized once at the `call_nqp_op` boundary, so
            // it shares this implementation (see `nqp_ops.rs`).
            // Cost: O(e) for an array/hash, e = elements; O(1) otherwise.
            "clone" | "clone_nd" => {
                let v = crate::runtime::types::unwrap_varref_value(
                    args.first().cloned().unwrap_or(Value::NIL),
                );
                // An object is copied by the native `Mu.clone` (never a user
                // `clone` method: nqp::clone is below the method layer), an
                // array by the same copy `.clone` makes (ADR-0118). This used
                // to hand back the SAME instance, sharing its attributes.
                if matches!(v.view(), ValueView::Instance { .. })
                    && let Some(r) = self.native_instance_clone_value(&v, &[])
                {
                    return Some(r);
                }
                if let Some(copy) = v.array_shallow_clone() {
                    return Some(Ok(copy));
                }
                Ok(match v.view() {
                    ValueView::Hash(map) => {
                        let copied: Vec<(String, Value)> =
                            map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                        let mut fresh = ValueMap::default();
                        for (k, v) in copied {
                            fresh.insert(k, v);
                        }
                        Value::hash_with_data(Value::hash_arc(fresh))
                    }
                    _ => v.clone(),
                })
            }

            _ => return self.call_nqp_op_list(op, args),
        })
    }
}
