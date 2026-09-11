//! The `nqp::` string and hash primitives.
//!
//! Third link in the pure-value chain (`nqp_ops` → `nqp_ops_process` →
//! `nqp_ops_text` → here), split for the 500-line limit.
//!
//! Every string op is **codepoint-indexed**, not byte-indexed: nqp's `substr`,
//! `index` and `chars` all count the same units, and nqp code mixes them
//! freely (`nqp::substr($s, 0, nqp::index($s, $needle))`). Indexing by bytes
//! would agree with rakudo on ASCII and diverge on the first accented
//! character.

use super::*;

fn sarg(args: &[Value], i: usize) -> String {
    args.get(i).map(|v| v.to_string_value()).unwrap_or_default()
}

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

/// The byte offset of codepoint `n` in `s`, or the string's length.
fn char_offset(s: &str, n: usize) -> usize {
    s.char_indices().nth(n).map(|(i, _)| i).unwrap_or(s.len())
}

impl Interpreter {
    /// Try a string / hash `nqp::` op. `None` means "not an op this table
    /// knows"; the caller then raises the unsupported-op error.
    pub(crate) fn call_nqp_op_str(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::substr($s, $from) / ($s, $from, $chars). A negative or
            // past-the-end `$from` clamps rather than dying, and a `$chars`
            // that runs past the end truncates — nqp's own behaviour, which
            // `String::Utils`'s scanners rely on (they walk with an index that
            // may reach `chars($s)`).
            "substr" => {
                let s = sarg(args, 0);
                let total = s.chars().count();
                let from = iarg(args, 1).max(0) as usize;
                let from = from.min(total);
                let want = if args.len() > 2 {
                    let n = iarg(args, 2);
                    if n < 0 { 0 } else { n as usize }
                } else {
                    total - from
                };
                let start = char_offset(&s, from);
                let end = char_offset(&s, from.saturating_add(want));
                Ok(Value::str(s[start..end].to_string()))
            }
            "concat" => Ok(Value::str(format!("{}{}", sarg(args, 0), sarg(args, 1)))),
            // nqp::index / rindex return **-1** when the needle is absent,
            // where Raku's `index` returns Nil. nqp code branches on exactly
            // that, so the -1 is the contract, not a placeholder.
            "index" | "rindex" => {
                let haystack = sarg(args, 0);
                let needle = sarg(args, 1);
                let chars: Vec<char> = haystack.chars().collect();
                let needle_chars: Vec<char> = needle.chars().collect();
                let from = if args.len() > 2 {
                    iarg(args, 2).max(0) as usize
                } else if op == "index" {
                    0
                } else {
                    chars.len()
                };
                let found = if needle_chars.is_empty() {
                    Some(from.min(chars.len()))
                } else if op == "index" {
                    (from..=chars.len().saturating_sub(needle_chars.len()))
                        .find(|&i| chars[i..i + needle_chars.len()] == needle_chars[..])
                } else {
                    let last = from.min(chars.len().saturating_sub(needle_chars.len()));
                    (0..=last)
                        .rev()
                        .find(|&i| chars[i..i + needle_chars.len()] == needle_chars[..])
                };
                Ok(Value::int(found.map(|i| i as i64).unwrap_or(-1)))
            }
            "flip" => Ok(Value::str(sarg(args, 0).chars().rev().collect::<String>())),
            "uc" => Ok(Value::str(sarg(args, 0).to_uppercase())),
            "lc" => Ok(Value::str(sarg(args, 0).to_lowercase())),
            "x" => {
                let n = iarg(args, 1);
                let n = if n < 0 { 0 } else { n as usize };
                Ok(Value::str(sarg(args, 0).repeat(n)))
            }

            // -- hash primitives --
            // These mutate the hash IN PLACE, through the shared `Gc`, because
            // that is what nqp callers assume: `abbrev` reaches for a Map's
            // storage once and then builds it with bindkey/deletekey, expecting
            // the Map it already holds to reflect every write.
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
            "clone" => {
                let v = crate::runtime::types::unwrap_varref_value(
                    args.first().cloned().unwrap_or(Value::NIL),
                );
                Ok(match v.view() {
                    ValueView::Array(items, _) => Value::array(items.to_vec()),
                    ValueView::Hash(map) => {
                        let copied: Vec<(String, Value)> =
                            map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                        let mut fresh = std::collections::HashMap::new();
                        for (k, v) in copied {
                            fresh.insert(k, v);
                        }
                        Value::hash_with_data(Value::hash_arc(fresh))
                    }
                    _ => v.clone(),
                })
            }

            _ => return None,
        })
    }
}
