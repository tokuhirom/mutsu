//! The `nqp::` text ops: character classes, Unicode properties, codepoint
//! arrays, and the native string/list primitives that go with them.
//!
//! Split out of `nqp_ops` for the 500-line limit, and
//! reached from the same dispatch chain: `call_nqp_op` falls through to
//! `call_nqp_op_process`, which falls through to `call_nqp_op_text` before the
//! loud unsupported-op error.
//!
//! The Unicode property ops (`unipropcode`, `getuniprop_*`, ...) live in
//! `nqp_ops_unicode`, with MoarVM's measured property and value codes in
//! `nqp_uniprop_data`: nqp code branches on those numbers directly —
//! `String::Utils`'s `nomark` compares a `getuniprop_int` result against a
//! bare `6` and means "Mn" by it (see `t/vm/nqp-text-unicode-ops.t`). The
//! `CCLASS_*` table itself lives in `builtins::cclass`, shared with the regex
//! engine.

pub(crate) use super::nqp_backing::push_elem;
use super::*;
use crate::builtins::cclass::is_cclass;

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn sarg(args: &[Value], i: usize) -> String {
    args.get(i).map(|v| v.to_string_value()).unwrap_or_default()
}

impl Interpreter {
    /// Try a text / Unicode / native-list `nqp::` op. `None` means "not an op
    /// this table knows"; the caller then raises the unsupported-op error.
    pub(crate) fn call_nqp_op_text(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // -- character classes --
            // nqp::iscclass($cclass, $str, $offset) -> 0/1 for ONE grapheme,
            // judged by its first codepoint (`str_prim::char_at`, the same
            // codepoint `nqp::ordat` reports).
            // Cost: O(1) amortized for a flat string, O(STRIDE) otherwise.
            "iscclass" => {
                let cclass = iarg(args, 0);
                let nil = Value::NIL;
                let src = args.get(1).unwrap_or(&nil);
                let yes = usize::try_from(iarg(args, 2)).is_ok_and(|g| {
                    crate::builtins::grapheme_index::with_str_index(src, |text, idx| {
                        crate::builtins::str_prim::char_at(text, idx, g)
                            .is_some_and(|c| is_cclass(cclass, c))
                    })
                });
                Ok(Value::int(yes as i64))
            }
            // nqp::findcclass / findnotcclass($cclass, $str, $offset, $count)
            // -> the index of the first (non-)member in the window, or the
            // window's END when there is none. Returning the end rather than
            // -1 is what lets `findnotcclass(...) == chars($s)` mean "the
            // whole string is of this class", which is how String::Utils's
            // `is-CCLASS` is written.
            // Cost: O(d), d = graphemes scanned.
            "findcclass" | "findnotcclass" => {
                let want = op == "findcclass";
                let cclass = iarg(args, 0);
                let found = crate::builtins::str_prim::find_char(
                    args.get(1).unwrap_or(&Value::NIL),
                    iarg(args, 2),
                    iarg(args, 3),
                    |c| is_cclass(cclass, c) == want,
                );
                Ok(Value::int(found as i64))
            }

            // -- codepoint arrays --
            // nqp::strtocodes($str, $normalization, $target) — decompose or
            // compose per the NORMALIZE_* mode, put the codepoints in $target,
            // and return it. The target is REPLACED, not appended to (measured
            // against rakudo), and mutated in place because nqp callers keep
            // their own reference to it: `String::Utils`'s `root` allocates one
            // buffer and re-fills it once per word, and an appending version
            // silently compared every word after the first against the wrong
            // offsets (`root <abcd abce abde>` answered "abc", not "ab").
            // Cost: O(n + k), n = chars of $str (copied, normalized), k = old elems of $target.
            "strtocodes" => {
                let target = args.get(2).cloned().unwrap_or(Value::NIL);
                super::nqp_ops_string::refill_codes(op, &sarg(args, 0), iarg(args, 1), &target)
                    .map(|()| target)
            }
            // nqp::strfromcodes($codes) -> the string those codepoints spell.
            // MoarVM strings are NFG, so building one from codepoints
            // normalizes: rakudo's `nqp::strfromcodes("bå".NFD)` is two
            // graphemes spelling `bå` precomposed, not the decomposed
            // sequence it was handed. Without this, `JSON::Fast`'s escaper
            // — which round-trips every string through `.NFD` and back —
            // emitted decomposed text for any composed input.
            // Cost: O(e), e = elems of $codes (array copied, string built, NFC-normalized).
            "strfromcodes" => {
                let codes = args.first().cloned().unwrap_or(Value::NIL);
                super::nqp_ops_string::codes_to_string(op, &codes).map(|out| {
                    Value::str(
                        match crate::builtins::str_prim::normalize(
                            &out,
                            crate::builtins::str_prim::Normal::Nfc,
                        ) {
                            std::borrow::Cow::Borrowed(_) => out,
                            std::borrow::Cow::Owned(nfc) => nfc,
                        },
                    )
                })
            }

            // -- string primitives --
            // nqp::eqat / nqp::eqatic($haystack, $needle, $pos) -> 1 when the
            // needle occurs at exactly grapheme `$pos` (case-folded for
            // `eqatic`). The same routine as `.starts-with` / `.substr-eq`.
            // Cost: O(m) amortized, m = chars of $needle.
            "eqat" | "eqatic" => {
                let fold = if op == "eqatic" {
                    crate::builtins::str_prim::Fold::Case
                } else {
                    crate::builtins::str_prim::Fold::Exact
                };
                let yes = crate::builtins::str_prim::nqp_eqat(
                    args.first().unwrap_or(&Value::NIL),
                    &sarg(args, 1),
                    iarg(args, 2),
                    fold,
                );
                Ok(Value::int(yes as i64))
            }

            // nqp::mod_i is MoarVM's, i.e. TRUNCATED like Rust's `%`
            // (`mod_i(-7, 3)` is -1), not Raku's floored `%`.
            // Cost: O(1).
            "mod_i" => crate::runtime::nqp_native::mod_i(iarg(args, 0), iarg(args, 1))
                .map(Value::int)
                .ok_or_else(|| RuntimeError::new("nqp::mod_i: division by zero")),

            // -- boxing / null --
            // nqp::hllbool($int) -> the HLL's Bool.
            // Cost: O(1).
            "hllbool" => Ok(if iarg(args, 0) != 0 {
                Value::TRUE
            } else {
                Value::FALSE
            }),
            // nqp::box_s($str, $type) -> a boxed string. mutsu's Str is not a
            // separate representation, so the type operand only has to be
            // honoured for a subclass, which `box_s` is never asked for here.
            // Cost: O(n), n = chars of $str (copied). MoarVM: O(1) -- see #9134.
            "box_s" => Ok(Value::str(sarg(args, 0))),
            // The VM-level null. mutsu has one absent value, so `null_s` and
            // `null` are both Nil.
            // Cost: O(1).
            "null_s" | "null" => Ok(Value::NIL),
            // TODO: compile a real null-string sentinel. MoarVM's `null_s` is
            // DISTINCT from the empty string, and the distinction is load-
            // bearing for the idiom this exists to serve: a `str` attribute is
            // set to `nqp::null_s` to mean "nothing buffered" and tested with
            // `isnull_s` (String::Utils's paragraph iterator). mutsu has no such
            // sentinel — storing Nil in a `str` attribute yields "" — so
            // `isnull_s` has to count "" as null here, which makes it answer 1
            // for a genuinely empty string too. The correct fix is a null-string
            // value that stays distinguishable through a native `str` slot.
            // Cost: O(1).
            "isnull" | "isnull_s" => {
                let v = args.first().cloned().unwrap_or(Value::NIL);
                let null = if op == "isnull_s" {
                    v.is_nil() || matches!(v.view(), ValueView::Str(s) if s.is_empty())
                } else {
                    v.is_nil()
                };
                Ok(Value::int(null as i64))
            }

            // -- native typed lists --
            // nqp::list_s(...) -> a VM list of strings; mutsu represents one as
            // an ordinary array, which is what atpos_s/bindpos_s/push_s below
            // (and atpos_i/bindpos_i) operate on. The array records its element
            // kind, so growing it fills with that type's zero (#9235).
            // Cost: O(k), k = operands.
            "list_s" | "list_i" | "list_n" => {
                let kind = match op {
                    "list_s" => crate::value::NqpElemKind::Str,
                    "list_i" => crate::value::NqpElemKind::Int,
                    _ => crate::value::NqpElemKind::Num,
                };
                Ok(Value::nqp_typed_list(args.to_vec(), kind))
            }
            // Cost: O(1) amortized (in-place push); push_s adds O(m), m = chars of the value
            // (copied). MoarVM: O(1) -- see #9134.
            "push_s" | "push_i" | "push_n" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let val = args.get(1).cloned().unwrap_or(Value::NIL);
                let val = match op {
                    "push_s" => Value::str(val.to_string_value()),
                    "push_n" => Value::num(val.to_f64()),
                    _ => Value::int(crate::runtime::to_int(&val)),
                };
                push_elem(op, &target, val)
            }
            // Cost: O(m), m = chars of the element (copied). MoarVM: O(1) -- see #9134.
            "atpos_s" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                match crate::runtime::nqp_backing::elem_at(&target, iarg(args, 1)) {
                    Ok(e) => Ok(Value::str(
                        e.map(|v| v.to_string_value()).unwrap_or_default(),
                    )),
                    Err(e) => Err(e),
                }
            }
            // Cost: O(m) + O(g), m = chars of the value (copied), g = slots grown past the end.
            // MoarVM: O(1) amortized -- see #9134.
            "bindpos_s" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let val = Value::str(sarg(args, 2));
                crate::runtime::nqp_backing::bind_elem(
                    op,
                    &target,
                    iarg(args, 1),
                    val,
                    Value::str(String::new()),
                )
            }

            // nqp::iterator / iterkey_s / iterval (`nqp_iter.rs`).
            // Cost: O(e) at worst, e = hash entries (each op states its own).
            _ if let Some(result) = super::nqp_iter::call_nqp_iter_op(op, args) => result,
            // Unicode properties and character names (`nqp_ops_unicode.rs`).
            // Cost: O(1) to O(S * m) (each op states its own).
            _ if let Some(result) = super::nqp_ops_unicode::call_nqp_unicode_op(op, args) => result,
            _ => return self.call_nqp_op_str(op, args),
        })
    }
}
