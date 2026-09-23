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

/// The "mark" (combining diacritic) stripped from a single codepoint: its
/// NFD decomposition's first non-combining part. `café`'s precomposed `é`
/// folds to `e`; a codepoint with no decomposition folds to itself. This is
/// a per-codepoint approximation, not a full grapheme fold, matching the
/// codepoint-indexed contract `nqp::indexim`/`nqp::indexicim` need: a
/// precomposed accented letter (the common case) stays 1:1 with the
/// original string, so comparing codepoint-for-codepoint keeps the returned
/// position aligned with `nqp::substr` on the same string.
fn strip_mark_char(c: char) -> char {
    use unicode_normalization::UnicodeNormalization;
    c.nfd()
        .find(|ch| !unicode_normalization::char::is_combining_mark(*ch))
        .unwrap_or(c)
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
            // Cost: O(k) cache hit, O(n) miss, k = chars returned, n = chars of $s.
            // MoarVM: O(k) -- see #9129.
            "substr" => {
                // Memoized (see `nqp_char_cache`): a hand-rolled NQP scanner
                // calls `nqp::substr($text, $pos, ...)` once per token over
                // the SAME full `$text` (JSON::Fast's own parser is the case
                // that surfaced this), so re-scanning it to a byte offset
                // via `char_indices` on every call was O(n) work repeated
                // O(n) times. Slicing the cached `Vec<char>` directly is
                // O(want) instead.
                let chars = super::nqp_char_cache::cached_chars(args, 0);
                let total = chars.len();
                let from = iarg(args, 1).max(0) as usize;
                let from = from.min(total);
                let want = if args.len() > 2 {
                    let n = iarg(args, 2);
                    if n < 0 { 0 } else { n as usize }
                } else {
                    total - from
                };
                let end = from.saturating_add(want).min(total);
                Ok(Value::str(chars[from..end].iter().collect::<String>()))
            }
            // Cost: O(n1 + n2), n1, n2 = chars of the operands.
            "concat" => Ok(Value::str(format!("{}{}", sarg(args, 0), sarg(args, 1)))),
            // nqp::index / rindex return **-1** when the needle is absent,
            // where Raku's `index` returns Nil. nqp code branches on exactly
            // that, so the -1 is the contract, not a placeholder.
            // Cost: O((n - from) * m) (+ O(n) on a char-cache miss), n = chars of
            // haystack, m = chars of needle. MoarVM: O((n - from) * m) -- see #9129.
            "index" | "rindex" => {
                let needle = sarg(args, 1);
                // The haystack is memoized (see `nqp_char_cache`): a
                // hand-rolled NQP scanner calls `nqp::index($text, needle,
                // $pos)` with the SAME full `$text` and an advancing `$pos`
                // (JSON::Fast's own string-token scan is the case that
                // surfaced this), so collecting it fresh on every call was
                // O(n) work repeated O(n) times.
                let chars = super::nqp_char_cache::cached_chars(args, 0);
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
            // nqp::indexic (case-insensitive), nqp::indexim (mark/diacritic-
            // insensitive), nqp::indexicim (both) — same -1-on-absent
            // contract as `index` above. `has-word`'s own case/mark folding
            // (`find-wordic`/`find-wordim`/`find-wordicim`) is what these
            // exist for.
            // Cost: O((n - from) * m) (+ O(n) on a char-cache miss), n = chars of
            // haystack, m = chars of needle. MoarVM: O((n - from) * m) -- see #9129.
            "indexic" | "indexim" | "indexicim" => {
                let needle = sarg(args, 1);
                let chars = super::nqp_char_cache::cached_chars(args, 0);
                let needle_chars: Vec<char> = needle.chars().collect();
                let from = if args.len() > 2 {
                    iarg(args, 2).max(0) as usize
                } else {
                    0
                };
                let char_eq = |a: char, b: char| -> bool {
                    match op {
                        "indexic" => a.to_lowercase().eq(b.to_lowercase()),
                        "indexicim" => strip_mark_char(a)
                            .to_lowercase()
                            .eq(strip_mark_char(b).to_lowercase()),
                        _ => strip_mark_char(a) == strip_mark_char(b), // "indexim"
                    }
                };
                let found = if needle_chars.is_empty() {
                    Some(from.min(chars.len()))
                } else if needle_chars.len() > chars.len() {
                    None
                } else {
                    (from..=chars.len() - needle_chars.len()).find(|&i| {
                        (0..needle_chars.len()).all(|k| char_eq(chars[i + k], needle_chars[k]))
                    })
                };
                Ok(Value::int(found.map(|i| i as i64).unwrap_or(-1)))
            }
            // Cost: O(n), n = chars of $s.
            "flip" => Ok(Value::str(sarg(args, 0).chars().rev().collect::<String>())),
            // Cost: O(n), n = chars of $s.
            "uc" => Ok(Value::str(sarg(args, 0).to_uppercase())),
            // Cost: O(n), n = chars of $s.
            "lc" => Ok(Value::str(sarg(args, 0).to_lowercase())),
            // Cost: O(n * c), n = chars of $s, c = repeat count.
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
                Ok(match v.view() {
                    ValueView::Array(items, _) => Value::array(items.to_vec()),
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
