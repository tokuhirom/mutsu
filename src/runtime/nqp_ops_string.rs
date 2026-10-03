//! The string `nqp::` ops of the coverage campaign's string slice (#11495):
//! case mapping (`tc`, `tclc`, `fc`), `codes`, the positional variants
//! (`indexfrom`, `rindexfrom`, `substr_s`, `replace`, `ordfirst`,
//! `ordbaseat`), `escape`, `sprintf` and its helpers, `unicmp_s`, `radix_I`,
//! and the codepoint-array / encoding ops (`normalizecodes`, `encode`,
//! `encodefromcodes`, `decodetocodes`).
//!
//! Each is the routine its Raku spelling already uses (ADR-0117): positions
//! are graphemes (`builtins::str_prim`), `fc`/`tclc`/`codes` are the `Str`
//! methods' bodies, `sprintf` is the formatter `sprintf`/`.fmt` run,
//! `unicmp_s` is `coll`'s collator, and `encode` is `Str.encode`'s encoder.

use crate::builtins::str_prim;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};
use std::borrow::Cow;

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn sarg(args: &[Value], i: usize) -> String {
    args.get(i).map(|v| v.to_string_value()).unwrap_or_default()
}

/// Operand `i`, or Nil when absent (a refcount bump, O(1)).
fn arg(args: &[Value], i: usize) -> Value {
    args.get(i).cloned().unwrap_or(Value::NIL)
}

/// `text` in the form an nqp `NORMALIZE_*` mode names; mode 0
/// (`NORMALIZE_NONE`) leaves it as it is.
// Cost: O(n), n = bytes of `text` (borrowed for ASCII and for mode 0).
fn normalized<'a>(op: &str, text: &'a str, mode: i64) -> Result<Cow<'a, str>, RuntimeError> {
    match str_prim::Normal::from_nqp_mode(mode) {
        Some(form) => Ok(str_prim::normalize(text, form)),
        None if mode == 0 => Ok(Cow::Borrowed(text)),
        None => Err(RuntimeError::new(format!(
            "nqp::{op}: unknown normalization mode {mode}"
        ))),
    }
}

/// Replace the elements of the nqp list / native array `target` with the
/// codepoints of `text` normalized per `mode`, in place (nqp callers keep
/// their own reference to the target): `strtocodes`, `normalizecodes` and
/// `decodetocodes` all fill their output this way.
// Cost: O(n + k), n = chars of `text`, k = old elems of `target`.
pub(super) fn refill_codes(
    op: &str,
    text: &str,
    mode: i64,
    target: &Value,
) -> Result<(), RuntimeError> {
    let codes = normalized(op, text, mode)?;
    Interpreter::nqp_with_elems_mut(op, target, |elems| {
        elems.clear();
        // Codepoints ARE the result here. str-prim: allow
        elems.extend(codes.chars().map(|ch| Value::int(ch as i64)));
    })
}

/// The string an nqp array of codepoints spells, NOT normalized (the
/// caller decides: `strfromcodes` composes to NFC, `normalizecodes` to the
/// form it was asked for).
// Cost: O(e), e = elems of `codes` (array copied, string built).
pub(super) fn codes_to_string(op: &str, codes: &Value) -> Result<String, RuntimeError> {
    let Some(elems) = Interpreter::nqp_elems_of(codes) else {
        return Err(RuntimeError::new(format!(
            "nqp::{op}: expected an array of codepoints"
        )));
    };
    let mut out = String::with_capacity(elems.len());
    for v in &elems {
        match u32::try_from(crate::runtime::to_int(v))
            .ok()
            .and_then(char::from_u32)
        {
            Some(ch) => out.push(ch),
            None => {
                return Err(RuntimeError::new(format!(
                    "nqp::{op}: {} is not a codepoint",
                    v.to_string_value()
                )));
            }
        }
    }
    Ok(out)
}

/// `nqp::escape`: NQP's string-literal escaping. A backslash, a double
/// quote and the seven named control characters are escaped; everything
/// else (other controls, `$`, `{`, non-ASCII) passes through (measured).
// Cost: O(n), n = chars of $s.
fn escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    // Escaping is per codepoint, not a string primitive. str-prim: allow
    for ch in s.chars() {
        let escaped = match ch {
            '\\' => "\\\\",
            '"' => "\\\"",
            '\u{7}' => "\\a",
            '\u{8}' => "\\b",
            '\t' => "\\t",
            '\n' => "\\n",
            '\u{C}' => "\\f",
            '\r' => "\\r",
            '\u{1B}' => "\\e",
            _ => {
                out.push(ch);
                continue;
            }
        };
        out.push_str(escaped);
    }
    out
}

/// `nqp::replace($s, $from, $count, $with)`, which MoarVM defines as
/// `substr($s, 0, $from) ~ $with ~ substr($s, $from + $count)` -- so a
/// negative `$from` keeps the whole string in front (`replace("abc", -1, 0,
/// "X")` is `abcXc`) and one below -1 dies on the negative length (measured).
// Cost: O(n + m), n = graphemes of $s, m = chars of $with.
fn replace(v: &Value, from: i64, count: i64, with: &Value) -> Result<Value, RuntimeError> {
    let head = str_prim::nqp_substr(v, 0, Some(from))?;
    let tail = str_prim::nqp_substr(v, from.saturating_add(count), None)?;
    Ok(str_prim::concat(str_prim::concat(head, with), &tail))
}

impl Interpreter {
    /// Try a string `nqp::` op of the #11495 slice. `None` means "not an op
    /// this table knows".
    pub(super) fn call_nqp_string_op(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // -- case mapping and counts: the `Str` methods' bodies --
            // Cost: O(n), n = chars of $s.
            "tc" => Ok(str_prim::titlecase_each(&sarg(args, 0))),
            // Cost: O(n), n = chars of $s (see method_table::str::tclc).
            "tclc" => crate::builtins::method_table::str::tclc(&arg(args, 0), &[]),
            // Cost: O(n), n = chars of $s (see method_table::str::fc).
            "fc" => crate::builtins::method_table::str::fc(&arg(args, 0), &[]),
            // nqp::codes($s): codepoints of the NFC form, which is the form
            // a mutsu string is held in (`"e\x[301]"` is 1).
            // Cost: O(n), n = bytes of $s (see method_table::str::codes).
            "codes" => crate::builtins::method_table::str::codes(&arg(args, 0), &[]),
            // A representation hint (MoarVM flattens a strand rope so later
            // indexing is O(1)); a mutsu string's grapheme index is already
            // built and cached on first use, so the string is returned as is.
            // Cost: O(1).
            "indexingoptimized" => Ok(arg(args, 0)),

            // -- positional variants of index / rindex / substr / ord --
            // Cost: O((n - from) * m), n = graphemes of haystack, m = chars of needle.
            "indexfrom" => Ok(Value::int(str_prim::nqp_index(
                &arg(args, 0),
                &sarg(args, 1),
                iarg(args, 2),
                str_prim::Fold::Exact,
            ))),
            // Cost: O((from - p) * m), p = the hit, m = chars of needle.
            "rindexfrom" => {
                str_prim::nqp_rindex(&arg(args, 0), &sarg(args, 1), Some(iarg(args, 2)))
                    .map(Value::int)
            }
            // Cost: O(k) amortized, k = graphemes returned.
            "substr_s" => str_prim::nqp_substr(&arg(args, 0), iarg(args, 1), Some(iarg(args, 2))),
            // Cost: O(n + m), n = graphemes of $s, m = chars of $with.
            "replace" => replace(&arg(args, 0), iarg(args, 1), iarg(args, 2), &arg(args, 3)),
            // nqp::ordfirst($s) is nqp::ordat($s, 0): -1 for the empty string.
            // Cost: O(1) amortized for a flat string, O(STRIDE) otherwise.
            "ordfirst" => Ok(Value::int(str_prim::nqp_ordat(&arg(args, 0), 0))),
            // Cost: O(g) amortized, g = chars of the grapheme.
            "ordbaseat" => Ok(Value::int(str_prim::nqp_ordbaseat(
                &arg(args, 0),
                iarg(args, 1),
            ))),
            // Cost: O(n), n = chars of $s.
            "escape" => Ok(Value::str(escape(&sarg(args, 0)))),

            // -- sprintf: the formatter `sprintf` and `.fmt` run --
            // nqp::sprintf($format, $args) with the arguments as an nqp list;
            // the directive-count and directive-type checks are `sprintf`'s.
            // Cost: O(f + o), f = bytes of $format, o = chars of the output.
            "sprintf" => {
                let fmt = sarg(args, 0);
                let items = Self::nqp_elems_of(&arg(args, 1)).unwrap_or_default();
                crate::runtime::sprintf::validate_sprintf_directives(&fmt, items.len())
                    .and_then(|()| {
                        crate::runtime::sprintf::validate_sprintf_arg_types(&fmt, &items)
                    })
                    .map(|()| {
                        Value::str(crate::runtime::sprintf::format_sprintf_args(&fmt, &items))
                    })
            }
            // Cost: O(f), f = bytes of $format.
            "sprintfdirectives" => Ok(Value::int(
                crate::runtime::sprintf::sprintf_sequential_count(&sarg(args, 0)) as i64,
            )),
            // NQP's own sprintf formats native values only and calls these
            // handlers to turn an HLL object into one; mutsu's formatter is
            // the HLL `sprintf`, which already formats every value itself, so
            // a handler has nothing left to convert. MoarVM answers "Added!".
            // Cost: O(1).
            "sprintfaddargumenthandler" => Ok(Value::str_from("Added!")),

            // nqp::unicmp_s($a, $b, $level, $iso, $country): `coll`'s collator
            // under the `$*COLLATION.collation-level` word `$level`. MoarVM
            // reserves the ISO language / country arguments and ignores them.
            // Cost: O(n1 + n2), n = chars of each operand (collation keys built).
            "unicmp_s" => {
                let settings =
                    crate::builtins::collation::CollationSettings::from_level(iarg(args, 2));
                let ord = crate::builtins::collation::coll_ordering(
                    &sarg(args, 0),
                    &sarg(args, 1),
                    &settings,
                );
                Ok(Value::int(ord as i64))
            }
            // Cost: O(k^2) bit operations, k = digits consumed.
            "radix_I" => super::nqp_radix::nqp_radix_big(args),

            // -- codepoint arrays and encodings --
            // nqp::normalizecodes($codes, $mode, $target) -> $target, refilled
            // with $codes in the form $mode names.
            // Cost: O(e + k), e = elems of $codes, k = old elems of $target.
            "normalizecodes" => {
                let target = arg(args, 2);
                codes_to_string(op, &arg(args, 0))
                    .and_then(|text| refill_codes(op, &text, iarg(args, 1), &target))
                    .map(|()| target)
            }
            // nqp::encode($s, $encoding, $buf) -> $buf with $s's encoded bytes
            // APPENDED (measured), through `Str.encode`'s encoder.
            // Cost: O(n + b), n = chars of $s, b = bytes appended.
            "encode" => {
                let target = arg(args, 2);
                self.nqp_encode_into(op, &sarg(args, 0), &sarg(args, 1), &target)
                    .map(|()| target)
            }
            // nqp::encodefromcodes($codes, $encoding, $buf): `encode` of the
            // string the codepoints spell, unnormalized.
            // Cost: O(e + b), e = elems of $codes, b = bytes appended.
            "encodefromcodes" => {
                let target = arg(args, 2);
                codes_to_string(op, &arg(args, 0))
                    .and_then(|text| self.nqp_encode_into(op, &text, &sarg(args, 1), &target))
                    .map(|()| target)
            }
            // nqp::decodetocodes($buf, $encoding, $mode, $target) -> $target,
            // refilled with the decoded codepoints in the form $mode names.
            // The decoder is `nqp::decode`'s (`Blob.decode`'s).
            // Cost: O(b + k), b = storage bytes of $buf, k = old elems of $target.
            "decodetocodes" => {
                let enc = sarg(args, 1);
                let target = arg(args, 3);
                super::nqp_ops::with_buf_storage_of(op, &arg(args, 0), |bytes, _| {
                    crate::builtins::decode_bytes_with_encoding_label(bytes, &enc).unwrap_or_else(
                        || {
                            Err(RuntimeError::new(format!(
                                "Unknown string encoding: '{enc}'"
                            )))
                        },
                    )
                })
                .and_then(|decoded| decoded)
                .and_then(|text| refill_codes(op, &text, iarg(args, 2), &target))
                .map(|()| target)
            }
            _ => return None,
        })
    }

    /// Append `text` encoded as `encoding` to the Buf `target`'s storage, the
    /// bytes read as elements of its own width (UTF-16 into a `buf16` is one
    /// code unit per element, as in MoarVM).
    // Cost: O(n + b), n = chars of `text`, b = bytes appended.
    fn nqp_encode_into(
        &self,
        op: &str,
        text: &str,
        encoding: &str,
        target: &Value,
    ) -> Result<(), RuntimeError> {
        let bytes = self.encode_with_encoding(text, encoding)?;
        super::nqp_ops::buf_storage_mutate(op, target, |storage, width| {
            if width == 0 || bytes.len() % width != 0 {
                return Err(RuntimeError::new(format!(
                    "nqp::{op}: {} bytes of {encoding} do not fill {width}-byte elements",
                    bytes.len()
                )));
            }
            storage.extend_from_slice(&bytes);
            Ok(())
        })
    }
}
