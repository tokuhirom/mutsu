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
use crate::builtins::unicode_gc::GeneralCategory;

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
    cclass & cclass_bits(ch) != 0
}

/// Every `CCLASS_*` bit `ch` belongs to, as one integer.
///
/// This is called once per character scanned by `nqp::findcclass` /
/// `findnotcclass` -- the inner loop of a hand-rolled NQP scanner such as
/// `JSON::Fast`'s `parse-string` -- so it answers all thirteen classes from a
/// single table load rather than re-deriving each one from string
/// comparisons against the category name.
fn cclass_bits(ch: char) -> i64 {
    use crate::builtins::unicode_gc::general_category;

    let mut bits = CCLASS_BY_GC[general_category(ch) as usize];
    let cp = ch as u32;
    // The members that are not a function of the General_Category: specific
    // codepoints, and the ASCII-only hexadecimal class.
    if ch.is_ascii_hexdigit() {
        bits |= 16; // CCLASS_HEXADECIMAL
    }
    if matches!(cp, 0x0A | 0x0B | 0x0C | 0x0D | 0x85) {
        bits |= 32 | 4096; // WHITESPACE, NEWLINE
    }
    if cp == 0x09 {
        bits |= 32 | 256; // WHITESPACE, BLANK
    }
    if cp == 0x5F {
        bits |= 8192; // CCLASS_WORD
    }
    bits
}

/// The `CCLASS_*` bits implied by a General_Category, indexed by its
/// discriminant. Derived from the rules documented at the top of this file;
/// `t/nqp/nqp-cclass-uniprop.t` pins the result against rakudo.
const CCLASS_BY_GC: [i64; GeneralCategory::ALL.len()] = {
    let mut table = [0i64; GeneralCategory::ALL.len()];
    let mut i = 0;
    while i < table.len() {
        table[i] = cclass_bits_for_gc(GeneralCategory::ALL[i]);
        i += 1;
    }
    table
};

const fn cclass_bits_for_gc(gc: GeneralCategory) -> i64 {
    let alphabetic = gc.in_mask(GeneralCategory::LETTER);
    let numeric = matches!(gc, GeneralCategory::Nd);
    let separator = gc.in_mask(GeneralCategory::SEPARATOR);
    let control = matches!(gc, GeneralCategory::Cc);
    let mut bits = 0i64;
    if matches!(gc, GeneralCategory::Lu) {
        bits |= 1; // CCLASS_UPPERCASE
    }
    if matches!(gc, GeneralCategory::Ll) {
        bits |= 2; // CCLASS_LOWERCASE
    }
    if alphabetic {
        bits |= 4; // CCLASS_ALPHABETIC
    }
    if numeric {
        bits |= 8; // CCLASS_NUMERIC
    }
    if separator {
        bits |= 32; // CCLASS_WHITESPACE
    }
    if !control {
        bits |= 64; // CCLASS_PRINTING
    }
    if matches!(gc, GeneralCategory::Zs) {
        bits |= 256; // CCLASS_BLANK
    }
    if control {
        bits |= 512; // CCLASS_CONTROL
    }
    if gc.in_mask(GeneralCategory::PUNCTUATION) {
        bits |= 1024; // CCLASS_PUNCTUATION
    }
    if alphabetic || numeric {
        bits |= 2048 | 8192; // CCLASS_ALPHANUMERIC, CCLASS_WORD
    }
    if matches!(gc, GeneralCategory::Zl | GeneralCategory::Zp) {
        bits |= 4096; // CCLASS_NEWLINE
    }
    bits
}

/// `(chars, offset)` for a cclass scan: nqp indexes strings by codepoint, so
/// every one of these ops works on a `Vec<char>` rather than on bytes. The
/// `args[1]` string is memoized across consecutive calls (see
/// `nqp_char_cache`) -- a hand-rolled NQP scanner calls `findcclass`/
/// `findnotcclass`/`iscclass` once per character over the SAME string.
fn scan_bounds(args: &[Value]) -> (std::rc::Rc<Vec<char>>, usize, usize) {
    let chars = super::nqp_char_cache::cached_chars(args, 1);
    let offset = iarg(args, 2).max(0) as usize;
    let count = iarg(args, 3).max(0) as usize;
    let end = offset.saturating_add(count).min(chars.len());
    (chars, offset.min(end), end)
}

/// Push a value onto an nqp list / native array in place. Returns the
/// pushed value itself (not the array) -- nqp's own `push`/`push_i`/
/// `push_s`/`push_n` all hand back the element just appended, which is what
/// lets idioms like `has-word`'s
/// `nqp::add_i(nqp::push_i(@positions,$pos),$move)` chain off it directly.
pub(crate) fn push_elem(op: &str, target: &Value, val: Value) -> Result<Value, RuntimeError> {
    // A Buf encodes just the new element onto its storage (#7680, #9132);
    // the generic element editor below would decode and re-encode it whole.
    if let Some((class_name, attrs)) = crate::value::value_buf::buf_target(target) {
        let end = crate::value::value_buf::BufEnd::Back;
        crate::value::value_buf::extend_buf_elems(
            &attrs,
            class_name,
            std::slice::from_ref(&val),
            end,
        );
        return Ok(val);
    }
    Interpreter::nqp_with_elems_mut(op, target, |elems| elems.push(val.clone()))?;
    Ok(val)
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
            // Cost: O(1) amortized on a nqp_char_cache hit; O(n) on a miss, n = chars of $str.
            // MoarVM: O(1) -- see #9129.
            "iscclass" => {
                let chars = super::nqp_char_cache::cached_chars(args, 1);
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
            // Cost: O(d) + O(n) on a nqp_char_cache miss, d = chars scanned, n = chars of $str.
            // MoarVM: O(d) -- see #9129.
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
            // Cost: O(m), m = chars of $name (copied, then compared).
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
            // Cost: O(1) (table lookup; astral codepoints O(log r) binary search, r = ranges).
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
                    .map(|ch| crate::builtins::unicode_gc::general_category(ch).as_str())
                    .unwrap_or("Cn");
                if op == "getuniprop_str" {
                    Ok(Value::str_from(gc))
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
            // Cost: O(n + k), n = chars of $str (copied, normalized), k = old elems of $target.
            "strtocodes" => {
                let text = sarg(args, 0);
                let mode = iarg(args, 1);
                let target = args.get(2).cloned().unwrap_or(Value::NIL);
                let normalized = match mode {
                    0 => text,
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
                let refilled = Self::nqp_with_elems_mut(op, &target, |elems| {
                    elems.clear();
                    elems.extend(normalized.chars().map(|ch| Value::int(ch as i64)));
                });
                match refilled {
                    Ok(()) => Ok(target),
                    Err(e) => Err(e),
                }
            }
            // nqp::strfromcodes($codes) -> the string those codepoints spell.
            // Cost: O(e), e = elems of $codes (array copied, string built, NFC-normalized).
            "strfromcodes" => {
                let codes = args.first().cloned().unwrap_or(Value::NIL);
                let Some(elems) = Self::nqp_elems_of(&codes) else {
                    return Some(Err(RuntimeError::new(
                        "nqp::strfromcodes: expected an array of codepoints".to_string(),
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
                            return Some(Err(RuntimeError::new(format!(
                                "nqp::strfromcodes: {} is not a codepoint",
                                v.to_string_value()
                            ))));
                        }
                    }
                }
                // MoarVM strings are NFG, so building one from codepoints
                // normalizes: rakudo's `nqp::strfromcodes("bå".NFD)` is two
                // graphemes spelling `bå` precomposed, not the decomposed
                // sequence it was handed. Without this, `JSON::Fast`'s escaper
                // — which round-trips every string through `.NFD` and back —
                // emitted decomposed text for any composed input.
                Ok(Value::str(normalize(&out, Normalization::Nfc)))
            }

            // -- string primitives --
            // nqp::eqatic($haystack, $needle, $pos) -> 1 when the needle
            // occurs at exactly codepoint offset `$pos`, ignoring case.
            // Cost: O(m) on a nqp_char_cache hit (haystack); O(n + m) on a miss,
            // n = chars of $haystack, m = chars of $needle. MoarVM: O(m) -- see #9129.
            "eqatic" => {
                let haystack = super::nqp_char_cache::cached_chars(args, 0);
                let needle: Vec<char> = sarg(args, 1).chars().collect();
                let pos = iarg(args, 2);
                let yes = usize::try_from(pos)
                    .ok()
                    .and_then(|p| haystack.get(p..p.saturating_add(needle.len())))
                    .map(|window| {
                        window
                            .iter()
                            .zip(&needle)
                            .all(|(left, right)| left.to_lowercase().eq(right.to_lowercase()))
                    })
                    .unwrap_or(false);
                Ok(Value::int(yes as i64))
            }
            // nqp::eqat($haystack, $needle, $pos) -> 1 when $needle occurs at
            // exactly codepoint offset $pos. The haystack is memoized (see
            // `nqp_char_cache`): JSON::Fast's string-token scan calls this
            // repeatedly against the SAME full document text.
            // Cost: O(m) on a nqp_char_cache hit (haystack); O(n + m) on a miss,
            // n = chars of $haystack, m = chars of $needle. MoarVM: O(m) -- see #9129.
            "eqat" => {
                let haystack = super::nqp_char_cache::cached_chars(args, 0);
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
            // Cost: O(1).
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

            // -- native string lists --
            // nqp::list_s(...) -> a VM list of strings; mutsu represents one as
            // an ordinary array, which is what atpos_s/bindpos_s/push_s below
            // (and the existing atpos_i/bindpos_i) already operate on.
            // Cost: O(k), k = operands.
            "list_s" | "list_i" | "list_n" => Ok(Value::array(args.to_vec())),
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
            // Cost: O(m) + O(g), m = chars of the value (copied), g = slots grown past the end.
            // MoarVM: O(1) amortized -- see #9134.
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
