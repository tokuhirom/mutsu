//! The `nqp::` text ops: character classes, Unicode properties, codepoint
//! arrays, and the native string/list primitives that go with them.
//!
//! Split out of `nqp_ops` for the 500-line limit, and
//! reached from the same dispatch chain: `call_nqp_op` falls through to
//! `call_nqp_op_process`, which falls through to `call_nqp_op_text` before the
//! loud unsupported-op error.
//!
//! **The numbers here are MoarVM's, measured against rakudo, not invented.**
//! nqp code branches on them directly — `String::Utils`'s `nomark` compares a
//! `getuniprop_int` result against a bare `6` and means "Mn" by it — so a
//! self-consistent numbering of our own would run such code silently wrong
//! rather than loudly unsupported. The General_Category value codes below were
//! read off rakudo by walking codepoints 0..0x2FFFF, and the `CCLASS_*`
//! membership rules by probing `nqp::iscclass` per class (see
//! `t/nqp/nqp-cclass-uniprop.t`, which pins both against the same values).

use super::*;

/// MoarVM's General_Category property value codes, in its enumeration order.
/// The index into this table *is* the value `nqp::getuniprop_int` returns.
const GENERAL_CATEGORY_CODES: [&str; 30] = [
    "Cn", "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Me", "Mc", "Nd", "Nl", "No", "Zs", "Zl", "Zp", "Cc",
    "Cf", "Co", "Cs", "Pd", "Ps", "Pe", "Pc", "Po", "Sm", "Sc", "Sk", "So", "Pi", "Pf",
];

/// MoarVM's property code for `General_Category`, as `nqp::unipropcode`
/// answers it. Only the properties mutsu can actually answer are listed: an
/// unknown name is an error rather than a handle that would later produce a
/// confidently wrong integer.
const PROP_GENERAL_CATEGORY: i64 = 20;

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn sarg(args: &[Value], i: usize) -> String {
    args.get(i).map(|v| v.to_string_value()).unwrap_or_default()
}

/// Is `ch` a member of the MoarVM character class `cclass` (a `CCLASS_*` bit)?
///
/// Every rule is derived from the General Category, which is what MoarVM's own
/// classes are defined over — note that `CCLASS_ALPHABETIC` is `L*`, NOT the
/// Unicode `Alphabetic` property (rakudo answers 0 for U+2160 ROMAN NUMERAL
/// ONE, which is `Nl` and `Alphabetic=Yes`), and `CCLASS_UPPERCASE` is `Lu`
/// rather than `Uppercase`, for the same reason.
fn is_cclass(cclass: i64, ch: char) -> bool {
    const ANY: i64 = 65535;
    if cclass == ANY {
        return true;
    }
    let gc = crate::builtins::unicode::unicode_general_category(ch);
    let gc = gc.as_str();
    let cp = ch as u32;
    // The horizontal/vertical space members that are not `Z*` categories.
    let is_line_break = matches!(cp, 0x0A | 0x0B | 0x0C | 0x0D | 0x85);
    let alphabetic = matches!(gc, "Lu" | "Ll" | "Lt" | "Lm" | "Lo");
    let numeric = gc == "Nd";

    let mut matched = false;
    let mut test = |bit: i64, yes: bool| {
        if cclass & bit != 0 && yes {
            matched = true;
        }
    };
    test(1, gc == "Lu"); // CCLASS_UPPERCASE
    test(2, gc == "Ll"); // CCLASS_LOWERCASE
    test(4, alphabetic); // CCLASS_ALPHABETIC
    test(8, numeric); // CCLASS_NUMERIC
    test(16, ch.is_ascii_hexdigit()); // CCLASS_HEXADECIMAL (ASCII only)
    test(
        32,
        matches!(gc, "Zs" | "Zl" | "Zp") || is_line_break || cp == 0x09,
    ); // WHITESPACE
    test(64, gc != "Cc"); // CCLASS_PRINTING
    test(256, gc == "Zs" || cp == 0x09); // CCLASS_BLANK
    test(512, gc == "Cc"); // CCLASS_CONTROL
    test(1024, gc.starts_with('P')); // CCLASS_PUNCTUATION
    test(2048, alphabetic || numeric); // CCLASS_ALPHANUMERIC
    test(4096, is_line_break || matches!(gc, "Zl" | "Zp")); // CCLASS_NEWLINE
    test(8192, alphabetic || numeric || cp == 0x5F); // CCLASS_WORD
    matched
}

/// `(chars, offset)` for a cclass scan: nqp indexes strings by codepoint, so
/// every one of these ops works on a `Vec<char>` rather than on bytes.
fn scan_bounds(args: &[Value]) -> (Vec<char>, usize, usize) {
    let chars: Vec<char> = sarg(args, 1).chars().collect();
    let offset = iarg(args, 2).max(0) as usize;
    let count = iarg(args, 3).max(0) as usize;
    let end = offset.saturating_add(count).min(chars.len());
    (chars, offset.min(end), end)
}

/// Empty an nqp list / native array in place.
fn clear_elems(op: &str, target: &Value) -> Result<(), RuntimeError> {
    match target.view() {
        ValueView::Array(items, _) => {
            // SAFETY: audited aliased in-place container write (see
            // value::aliased_mut); no borrow into the node is live.
            let data = unsafe { crate::value::gc_contents_mut(&items) };
            data.items_mut().clear();
            Ok(())
        }
        ValueView::Instance { attributes, .. } => {
            let done = crate::value::value_buf::with_buf_elems_mut(&attributes, |e| e.clear());
            if done.is_none() {
                return Err(RuntimeError::new(format!(
                    "nqp::{op}: expected a Buf/Blob or array"
                )));
            }
            Ok(())
        }
        _ => Err(RuntimeError::new(format!(
            "nqp::{op}: expected a Buf/Blob or array"
        ))),
    }
}

/// Push a value onto an nqp list / native array in place.
fn push_elem(op: &str, target: &Value, val: Value) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Array(items, _) => {
            // SAFETY: audited aliased in-place container write (see
            // value::aliased_mut) — the same pattern `bindpos_i` uses; no
            // borrow into the node is live.
            let data = unsafe { crate::value::gc_contents_mut(&items) };
            data.items_mut().push(val);
            Ok(target.clone())
        }
        ValueView::Instance { attributes, .. } => {
            let stored = val.clone();
            let done = crate::value::value_buf::with_buf_elems_mut(&attributes, |elems| {
                elems.push(stored)
            });
            if done.is_none() {
                return Err(RuntimeError::new(format!(
                    "nqp::{op}: expected a Buf/Blob or array"
                )));
            }
            Ok(target.clone())
        }
        _ => Err(RuntimeError::new(format!(
            "nqp::{op}: expected a Buf/Blob or array"
        ))),
    }
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
            // nqp::iscclass($cclass, $str, $offset) -> 0/1 for ONE character.
            "iscclass" => {
                let chars: Vec<char> = sarg(args, 1).chars().collect();
                let idx = iarg(args, 2).max(0) as usize;
                let yes = chars
                    .get(idx)
                    .map(|&c| is_cclass(iarg(args, 0), c))
                    .unwrap_or(false);
                Ok(Value::int(yes as i64))
            }
            // nqp::findcclass / findnotcclass($cclass, $str, $offset, $count)
            // -> the index of the first (non-)member in the window, or the
            // window's END when there is none. Returning the end rather than
            // -1 is what lets `findnotcclass(...) == chars($s)` mean "the
            // whole string is of this class", which is how String::Utils's
            // `is-CCLASS` is written.
            "findcclass" | "findnotcclass" => {
                let want = op == "findcclass";
                let cclass = iarg(args, 0);
                let (chars, start, end) = scan_bounds(args);
                let found = chars[start..end]
                    .iter()
                    .position(|&c| is_cclass(cclass, c) == want)
                    .map(|i| start + i)
                    .unwrap_or(end);
                Ok(Value::int(found as i64))
            }

            // -- Unicode properties --
            // nqp::unipropcode($name) -> the property handle getuniprop_* takes.
            "unipropcode" => {
                let name = sarg(args, 0);
                if name.eq_ignore_ascii_case("General_Category") || name.eq_ignore_ascii_case("gc")
                {
                    Ok(Value::int(PROP_GENERAL_CATEGORY))
                } else {
                    Err(RuntimeError::new(format!(
                        "nqp::unipropcode: unsupported property '{name}' \
                         (mutsu answers General_Category only)"
                    )))
                }
            }
            // nqp::getuniprop_int($codepoint, $propcode) -> the property VALUE
            // code, and nqp::getuniprop_str the same value as its name.
            "getuniprop_int" | "getuniprop_str" => {
                let cp = iarg(args, 0);
                let prop = iarg(args, 1);
                if prop != PROP_GENERAL_CATEGORY {
                    return Some(Err(RuntimeError::new(format!(
                        "nqp::{op}: unsupported property code {prop} \
                         (mutsu answers General_Category only)"
                    ))));
                }
                let gc = u32::try_from(cp)
                    .ok()
                    .and_then(char::from_u32)
                    .map(crate::builtins::unicode::unicode_general_category)
                    .unwrap_or_else(|| "Cn".to_string());
                if op == "getuniprop_str" {
                    Ok(Value::str(gc))
                } else {
                    let code = GENERAL_CATEGORY_CODES
                        .iter()
                        .position(|&c| c == gc)
                        .unwrap_or(0);
                    Ok(Value::int(code as i64))
                }
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
            "strtocodes" => {
                let text = sarg(args, 0);
                let mode = iarg(args, 1);
                let target = args.get(2).cloned().unwrap_or(Value::NIL);
                let normalized = match mode {
                    0 => text.clone(),
                    1 => normalize(&text, Normalization::Nfc),
                    2 => normalize(&text, Normalization::Nfd),
                    3 => normalize(&text, Normalization::Nfkc),
                    4 => normalize(&text, Normalization::Nfkd),
                    other => {
                        return Some(Err(RuntimeError::new(format!(
                            "nqp::strtocodes: unknown normalization mode {other}"
                        ))));
                    }
                };
                if let Err(e) = clear_elems(op, &target) {
                    return Some(Err(e));
                }
                for ch in normalized.chars() {
                    if let Err(e) = push_elem(op, &target, Value::int(ch as i64)) {
                        return Some(Err(e));
                    }
                }
                Ok(target)
            }
            // nqp::strfromcodes($codes) -> the string those codepoints spell.
            "strfromcodes" => {
                let codes = args.first().cloned().unwrap_or(Value::NIL);
                let elems: Vec<Value> = match codes.view() {
                    ValueView::Array(items, _) => items.to_vec(),
                    ValueView::Instance { attributes, .. } => {
                        crate::value::value_buf::with_buf_elems(&attributes, |e| e.to_vec())
                            .unwrap_or_default()
                    }
                    _ => {
                        return Some(Err(RuntimeError::new(
                            "nqp::strfromcodes: expected an array of codepoints".to_string(),
                        )));
                    }
                };
                let mut out = String::with_capacity(elems.len());
                for v in &elems {
                    match u32::try_from(crate::runtime::to_int(v))
                        .ok()
                        .and_then(char::from_u32)
                    {
                        Some(ch) => out.push(ch),
                        None => {
                            return Some(Err(RuntimeError::new(format!(
                                "nqp::strfromcodes: {} is not a codepoint",
                                v.to_string_value()
                            ))));
                        }
                    }
                }
                Ok(Value::str(out))
            }

            // -- string primitives --
            // nqp::eqat($haystack, $needle, $pos) -> 1 when $needle occurs at
            // exactly codepoint offset $pos.
            "eqat" => {
                let haystack: Vec<char> = sarg(args, 0).chars().collect();
                let needle: Vec<char> = sarg(args, 1).chars().collect();
                let pos = iarg(args, 2);
                let yes = usize::try_from(pos)
                    .ok()
                    .and_then(|p| haystack.get(p..p.saturating_add(needle.len())))
                    .map(|window| window == needle.as_slice())
                    .unwrap_or(false);
                Ok(Value::int(yes as i64))
            }

            // nqp::mod_i is MoarVM's, i.e. TRUNCATED like Rust's `%`
            // (`mod_i(-7, 3)` is -1), not Raku's floored `%`.
            "mod_i" => {
                let rhs = iarg(args, 1);
                if rhs == 0 {
                    return Some(Err(RuntimeError::new(
                        "nqp::mod_i: division by zero".to_string(),
                    )));
                }
                Ok(Value::int(iarg(args, 0).wrapping_rem(rhs)))
            }

            // -- boxing / null --
            // nqp::hllbool($int) -> the HLL's Bool.
            "hllbool" => Ok(if iarg(args, 0) != 0 {
                Value::TRUE
            } else {
                Value::FALSE
            }),
            // nqp::box_s($str, $type) -> a boxed string. mutsu's Str is not a
            // separate representation, so the type operand only has to be
            // honoured for a subclass, which `box_s` is never asked for here.
            "box_s" => Ok(Value::str(sarg(args, 0))),
            // The VM-level null. mutsu has one absent value, so `null_s` and
            // `null` are both Nil.
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
            "isnull" | "isnull_s" => {
                let v = args.first().cloned().unwrap_or(Value::NIL);
                let null = if op == "isnull_s" {
                    v.is_nil() || matches!(v.view(), ValueView::Str(s) if s.is_empty())
                } else {
                    v.is_nil()
                };
                Ok(Value::int(null as i64))
            }

            // -- native string lists --
            // nqp::list_s(...) -> a VM list of strings; mutsu represents one as
            // an ordinary array, which is what atpos_s/bindpos_s/push_s below
            // (and the existing atpos_i/bindpos_i) already operate on.
            "list_s" | "list_i" | "list_n" => Ok(Value::array(args.to_vec())),
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
            "atpos_s" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let idx = iarg(args, 1);
                let elem = match target.view() {
                    ValueView::Array(items, _) => usize::try_from(idx)
                        .ok()
                        .and_then(|i| items.get(i).cloned()),
                    ValueView::Instance { attributes, .. } => {
                        usize::try_from(idx).ok().and_then(|i| {
                            crate::value::value_buf::with_buf_elems(&attributes, |e| {
                                e.get(i).cloned()
                            })
                            .flatten()
                        })
                    }
                    _ => None,
                };
                Ok(Value::str(
                    elem.map(|v| v.to_string_value()).unwrap_or_default(),
                ))
            }
            "bindpos_s" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let idx = iarg(args, 1).max(0) as usize;
                let val = Value::str(sarg(args, 2));
                match target.view() {
                    ValueView::Array(items, _) => {
                        // SAFETY: audited aliased in-place container write (see
                        // value::aliased_mut); no borrow into the node is live.
                        let data = unsafe { crate::value::gc_contents_mut(&items) };
                        if data.items().len() <= idx {
                            data.items_mut().resize(idx + 1, Value::str(String::new()));
                        }
                        data.items_mut()[idx] = val.clone();
                        Ok(val)
                    }
                    ValueView::Instance { attributes, .. } => {
                        let stored = val.clone();
                        let done =
                            crate::value::value_buf::with_buf_elems_mut(&attributes, |elems| {
                                if elems.len() <= idx {
                                    elems.resize(idx + 1, Value::str(String::new()));
                                }
                                elems[idx] = stored;
                            });
                        if done.is_none() {
                            return Some(Err(RuntimeError::new(
                                "nqp::bindpos_s: expected a Buf/Blob or array".to_string(),
                            )));
                        }
                        Ok(val)
                    }
                    _ => Err(RuntimeError::new(
                        "nqp::bindpos_s: expected a Buf/Blob or array".to_string(),
                    )),
                }
            }

            _ => return self.call_nqp_op_str(op, args),
        })
    }
}

enum Normalization {
    Nfc,
    Nfd,
    Nfkc,
    Nfkd,
}

fn normalize(text: &str, form: Normalization) -> String {
    use unicode_normalization::UnicodeNormalization;
    match form {
        Normalization::Nfc => text.nfc().collect(),
        Normalization::Nfd => text.nfd().collect(),
        Normalization::Nfkc => text.nfkc().collect(),
        Normalization::Nfkd => text.nfkd().collect(),
    }
}
