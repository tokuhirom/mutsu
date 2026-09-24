//! The `nqp::` VALUE ops mutsu supports beyond the handful in `builtins.rs`.
//!
//! Dispatched from the `builtins_operators_fallback.rs` unsupported-op guard,
//! so an op missing here still fails loudly instead of silently reaching a
//! same-named Raku builtin. The CONTROL-FLOW ops (`nqp::if`, `nqp::while`,
//! `nqp::stmts`, `nqp::unless`, `nqp::until`) are special forms and compile
//! to jumps in `compiler/nqp_forms.rs`; the `nqp::const::*` flag constants
//! fold to integer literals there too.
//!
//! The immediate driver is `CBOR::Simple` (a hard dep of `Log::Timeline`,
//! itself a hard dep of Cro::HTTP), whose encoder/decoder is written almost
//! entirely in these ops — see `todo/tickets/cbor-simple-nqp-buf-ops.md`.

use crate::builtins::mvm_array_read_buf_oob_message;
use crate::runtime::nqp_pure::NqpPure;
use crate::runtime::{Interpreter, RuntimeError, path_is_readable};
use crate::value::value_buf;
use crate::value::{Value, ValueView};
use std::fs;

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn narg(args: &[Value], i: usize) -> f64 {
    args.get(i).map(|v| v.to_f64()).unwrap_or(0.0)
}

fn bool_int(b: bool) -> Value {
    Value::int(i64::from(b))
}

/// An op whose body lives in [`crate::runtime::nqp_pure`] — the single
/// implementation this string-keyed table and `exec_nqp_op`'s direct path
/// share (#8900).
#[inline]
fn pure(op: NqpPure, args: &[Value]) -> Value {
    crate::runtime::nqp_pure::eval(op, args)
}

/// `strtol`-style leading-integer parse for `nqp::coerce_si`: skip leading
/// whitespace, an optional sign, then digits; no digits at all is 0, and a
/// magnitude too large for `i64` saturates rather than erroring (matches
/// MoarVM's behavior, verified against `raku -e 'nqp::coerce_si(...)'`).
fn parse_leading_int(s: &str) -> i64 {
    let trimmed = s.trim_start();
    // Parsing ASCII digits, not indexing the string. str-prim: allow
    let mut chars = trimmed.chars().peekable();
    let negative = match chars.peek() {
        Some('-') => {
            chars.next();
            true
        }
        Some('+') => {
            chars.next();
            false
        }
        _ => false,
    };
    let digits: String = chars.take_while(char::is_ascii_digit).collect();
    if digits.is_empty() {
        return 0;
    }
    let magnitude: i128 = digits.parse().unwrap_or(i128::MAX);
    let signed = if negative { -magnitude } else { magnitude };
    signed.clamp(i64::MIN as i128, i64::MAX as i128) as i64
}

fn nqp_radix_digit(ch: char, radix: u32) -> Option<i64> {
    ch.to_digit(radix).map(i64::from)
}

/// Parse the native-int form of `nqp::radix`.
///
/// Rakudo returns an array containing the wrapped native result, the number of
/// result digits, and the codepoint offset after the consumed input. The
/// fourth argument is a flag word: bit 0 forces a negative result, bit 1
/// parses a leading sign, and bit 2 drops trailing zeroes from the result
/// while still consuming them. A native integer wraps on overflow, as does
/// the MoarVM operation.
fn nqp_radix(args: &[Value]) -> Result<Value, RuntimeError> {
    let radix = iarg(args, 0);
    let Ok(radix) = u32::try_from(radix) else {
        return Err(RuntimeError::new("nqp::radix: radix must be in 2..36"));
    };
    if !(2..=36).contains(&radix) {
        return Err(RuntimeError::new("nqp::radix: radix must be in 2..36"));
    }

    let source = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
    // One char per grapheme: `$pos` and the returned offset are grapheme
    // positions, like every other nqp string op.
    let chars = crate::builtins::str_prim::grapheme_base_chars(&source);
    let mut pos = iarg(args, 2).max(0) as usize;
    if pos >= chars.len() {
        return Ok(Value::array(vec![
            Value::int(0),
            Value::int(0),
            Value::int(-1),
        ]));
    }

    let flags = iarg(args, 3);
    let parse_sign = flags & 0x02 != 0;
    let mut negative = flags & 0x01 != 0;
    if parse_sign {
        match chars[pos] {
            '-' => {
                negative = true;
                pos += 1;
            }
            '+' => pos += 1,
            _ => {}
        }
    }

    let mut digits = Vec::new();
    let mut cursor = pos;
    while cursor < chars.len() {
        if let Some(digit) = nqp_radix_digit(chars[cursor], radix) {
            digits.push(digit);
            cursor += 1;
            continue;
        }
        // NQP permits a single underscore between two digits. It is consumed
        // but does not contribute to either the result or its digit count.
        if chars[cursor] == '_'
            && !digits.is_empty()
            && cursor + 1 < chars.len()
            && nqp_radix_digit(chars[cursor + 1], radix).is_some()
        {
            cursor += 1;
            continue;
        }
        break;
    }

    if digits.is_empty() {
        return Ok(Value::array(vec![
            Value::int(0),
            Value::int(0),
            Value::int(-1),
        ]));
    }

    let mut result_digits = digits.len();
    if flags & 0x04 != 0 {
        while result_digits > 0 && digits[result_digits - 1] == 0 {
            result_digits -= 1;
        }
    }
    let mut result = 0i64;
    for &digit in &digits[..result_digits] {
        // Digit accumulation into a native int, wrapping as MoarVM's does.
        // native-prim: allow
        result = result.wrapping_mul(radix as i64).wrapping_add(digit);
    }
    if negative {
        // native-prim: allow
        result = result.wrapping_neg();
    }

    Ok(Value::array(vec![
        Value::int(result),
        Value::int(result_digits as i64),
        Value::int(cursor as i64),
    ]))
}

fn cmp_result(ordering: std::cmp::Ordering) -> i64 {
    match ordering {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Binary read/write flag decoding (see `nqp_const_value`): the low 2 bits
/// are the endianness (0 native / 1 little / 2 big — Raku's `Endian` enum),
/// the bits above select the size as `1 << (flags >> 2)` bytes.
fn flag_size_endian(flags: i64) -> (usize, i64) {
    (1usize << ((flags >> 2).clamp(0, 4)), flags & 3)
}

/// The write-int method name `buf_write_int::apply_write_int` dispatches on,
/// for a byte size decoded from an nqp flag.
fn write_method_for(size: usize, signed: bool) -> &'static str {
    match (size, signed) {
        (1, false) => "write-uint8",
        (2, false) => "write-uint16",
        (4, false) => "write-uint32",
        (8, false) => "write-uint64",
        (1, true) => "write-int8",
        (2, true) => "write-int16",
        (4, true) => "write-int32",
        _ => "write-int64",
    }
}

/// Run `f` over the bytes of a Buf/Blob instance, borrowed (no copy for a
/// width-1 buffer), or error naming the op.
fn with_buf_bytes_of<R>(
    op: &str,
    v: &Value,
    f: impl FnOnce(&[u8]) -> R,
) -> Result<R, RuntimeError> {
    if let ValueView::Instance { attributes, .. } = v.view()
        && let Some(r) = value_buf::with_buf_bytes(&attributes, f)
    {
        return Ok(r);
    }
    Err(RuntimeError::new(format!(
        "nqp::{op}: expected a Buf/Blob, got {}",
        crate::runtime::value_type_name(v)
    )))
}

/// Mutate a Buf instance's bytes in place through its shared attribute cell
/// (alias-visible), or error naming the op. On a width-1 buffer this edits the
/// storage directly, so it costs only what `f` touches (see
/// [`value_buf::with_buf_bytes_mut`]).
fn buf_bytes_mutate(
    op: &str,
    v: &Value,
    f: impl FnOnce(&mut Vec<u8>) -> Result<(), RuntimeError>,
) -> Result<(), RuntimeError> {
    if let ValueView::Instance { attributes, .. } = v.view()
        && let Some(r) = value_buf::with_buf_bytes_mut(&attributes, f)
    {
        return r;
    }
    Err(RuntimeError::new(format!(
        "nqp::{op}: expected a Buf/Blob, got {}",
        crate::runtime::value_type_name(v)
    )))
}

impl Interpreter {
    /// Try an `nqp::` value op. `None` means "not an op this table knows" —
    /// the caller then raises the loud unsupported-op error.
    pub(crate) fn call_nqp_op(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // The `nqp::` layer has no notion of a Raku container: every op here
        // reads its operands as raw values, and the ones that inspect a value
        // *structurally* (`istype`, `elems`, `atpos_*`, `chars`, ...) answer
        // about the container itself rather than what it holds when handed one.
        // ADR-0036/ADR-0045 hand out element containers from `for` loop
        // bindings and the element producers, so `nqp::istype($_, Associative)`
        // inside `encode($_) for @$_` saw a `ContainerRef` and answered False —
        // CBOR::Simple then encoded a Map as its element count. Decontainerize
        // once at the boundary rather than op by op; the in-place mutators
        // (`bindpos_*`, `write*`, `splice`) reach their Buf/array through the
        // shared `Gc` behind the value, which survives the deref.
        let derefed;
        let args = if args
            .iter()
            .any(|v| v.is_container_ref() || v.is_hash_entry_ref_value())
        {
            derefed = args.iter().map(Value::deref_container).collect::<Vec<_>>();
            derefed.as_slice()
        } else {
            args
        };
        Some(match op {
            // -- native int arithmetic / bit ops --
            //
            // The pure ops below delegate to `nqp_pure::eval`, which is also
            // what `exec_nqp_op` runs when it takes the direct path (#8900).
            // One implementation, so the two paths cannot drift.
            // Cost: O(1).
            "add_i" => Ok(pure(NqpPure::AddI, args)),
            // Cost: O(1).
            "sub_i" => Ok(pure(NqpPure::SubI, args)),
            // Cost: O(1).
            "mul_i" => Ok(pure(NqpPure::MulI, args)),
            // nqp::div_i uses floor division, unlike Rust's `/` for negative
            // operands.  Array::Sorted::Util uses this to choose the midpoint
            // of its binary search, so this is observable in ordinary module
            // code rather than only in low-level NQP callers.
            // Cost: O(1).
            "div_i" => {
                let lhs = iarg(args, 0);
                let rhs = iarg(args, 1);
                crate::runtime::nqp_native::div_i(lhs, rhs)
                    .map(Value::int)
                    .ok_or_else(|| RuntimeError::new("nqp::div_i: division by zero"))
            }
            // Cost: O(1).
            "neg_i" => Ok(pure(NqpPure::NegI, args)),
            // Cost: O(1).
            "abs_i" => Ok(pure(NqpPure::AbsI, args)),
            // Cost: O(1).
            "bitor_i" => Ok(pure(NqpPure::BitOrI, args)),
            // Cost: O(1).
            "bitand_i" => Ok(pure(NqpPure::BitAndI, args)),
            // Cost: O(1).
            "bitxor_i" => Ok(pure(NqpPure::BitXorI, args)),
            // Cost: O(1).
            "bitneg_i" => Ok(pure(NqpPure::BitNegI, args)),
            // Cost: O(1).
            "bitshiftl_i" => Ok(pure(NqpPure::ShlI, args)),
            // Cost: O(1).
            "bitshiftr_i" => Ok(pure(NqpPure::ShrI, args)),
            // Arbitrary-precision add: nqp::add_I($a, $b, Int) — the third
            // argument is the boxing target type and is ignored here.
            // Cost: O(d), d = digits of the larger operand (both converted to BigInt).
            "add_I" => Ok(Value::from_bigint(
                args.first().map(|v| v.to_bigint()).unwrap_or_default()
                    + args.get(1).map(|v| v.to_bigint()).unwrap_or_default(),
            )),
            // Cost: O(d), d = digits of the larger operand (both converted to BigInt).
            "sub_I" => Ok(Value::from_bigint(
                args.first().map(|v| v.to_bigint()).unwrap_or_default()
                    - args.get(1).map(|v| v.to_bigint()).unwrap_or_default(),
            )),

            // -- native int comparisons (yield int 0/1, as in nqp) --
            // Cost: O(1).
            "iseq_i" => Ok(pure(NqpPure::IsEqI, args)),
            // Cost: O(1).
            "isne_i" => Ok(pure(NqpPure::IsNeI, args)),
            // Cost: O(1).
            "islt_i" => Ok(pure(NqpPure::IsLtI, args)),
            // Cost: O(1).
            "isle_i" => Ok(pure(NqpPure::IsLeI, args)),
            // Cost: O(1).
            "isgt_i" => Ok(pure(NqpPure::IsGtI, args)),
            // Cost: O(1).
            "isge_i" => Ok(pure(NqpPure::IsGeI, args)),
            // Cost: O(1).
            "cmp_i" => Ok(pure(NqpPure::CmpI, args)),
            // Cost: O(1).
            "not_i" => Ok(pure(NqpPure::NotI, args)),

            // -- native num arithmetic --
            // Cost: O(1).
            "add_n" => Ok(pure(NqpPure::AddN, args)),
            // Cost: O(1).
            "sub_n" => Ok(pure(NqpPure::SubN, args)),
            // Cost: O(1).
            "mul_n" => Ok(pure(NqpPure::MulN, args)),
            // Cost: O(1).
            "div_n" => Ok(pure(NqpPure::DivN, args)),
            // Cost: O(1).
            "neg_n" => Ok(pure(NqpPure::NegN, args)),
            // Cost: O(1).
            "abs_n" => Ok(pure(NqpPure::AbsN, args)),

            // nqp::radix($radix, $str, $pos, $flags) returns the wrapped
            // native-int result, the number of significant digits, and the
            // offset after consuming the input.
            // Cost: O(n), n = chars of $str (copied and fully collected into a Vec<char>
            // regardless of $pos). MoarVM: O(k), k = digits consumed from $pos -- see #9131.
            "radix" => nqp_radix(args),

            // -- native num comparisons --
            // Cost: O(1).
            "iseq_n" => Ok(pure(NqpPure::IsEqN, args)),
            // Cost: O(1).
            "isne_n" => Ok(pure(NqpPure::IsNeN, args)),
            // Cost: O(1).
            "islt_n" => Ok(pure(NqpPure::IsLtN, args)),
            // Cost: O(1).
            "isle_n" => Ok(pure(NqpPure::IsLeN, args)),
            // Cost: O(1).
            "isgt_n" => Ok(pure(NqpPure::IsGtN, args)),
            // Cost: O(1).
            "isge_n" => Ok(pure(NqpPure::IsGeN, args)),
            // Cost: O(1).
            "cmp_n" => Ok(pure(NqpPure::CmpN, args)),
            // Cost: O(1).
            "isnanorinf" => Ok(pure(NqpPure::IsNanOrInf, args)),

            // -- native str comparison --
            // Cost: O(n1+n2), n1/n2 = chars of the operands (both copied).
            "cmp_s" => {
                let lhs = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let rhs = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                Ok(Value::int(cmp_result(lhs.cmp(&rhs))))
            }
            // Cost: O(n1+n2), n1/n2 = chars of the operands (both copied first). MoarVM: O(1) on
            // differing lengths, else O(n) -- see #9134.
            "iseq_s" => Ok(bool_int(
                args.first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default()
                    == args.get(1).map(|v| v.to_string_value()).unwrap_or_default(),
            )),
            // Cost: O(n1+n2), n1/n2 = chars of the operands (both copied first). MoarVM: O(1) on
            // differing lengths, else O(n) -- see #9134.
            "isne_s" => Ok(bool_int(
                args.first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default()
                    != args.get(1).map(|v| v.to_string_value()).unwrap_or_default(),
            )),

            // `nqp::stat` follows the process cwd (or the interpreter's
            // configured cwd) and reports the basic filesystem predicates
            // needed by the standard `paths` module. `metadata` follows
            // symlinks, matching the POSIX stat operation rather than lstat.
            // Cost: O(p) + one syscall, p = length of $path.
            "stat" => {
                let path = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let code = iarg(args, 1);
                let path_buf = self.resolve_path(&path);
                let result = match code {
                    0 => i64::from(path_buf.exists()), // STAT_EXISTS
                    1..=3 => {
                        let metadata = match fs::metadata(&path_buf) {
                            Ok(metadata) => metadata,
                            Err(_) => {
                                return Some(Err(RuntimeError::new(format!(
                                    "Failed to stat file: {path}"
                                ))));
                            }
                        };
                        match code {
                            1 => metadata.len() as i64,         // STAT_FILESIZE
                            2 => i64::from(metadata.is_dir()),  // STAT_ISDIR
                            3 => i64::from(metadata.is_file()), // STAT_ISREG
                            _ => -1,
                        }
                    }
                    _ => -1,
                };
                Ok(Value::int(result))
            }

            // Directory handles are represented as ordinary opaque instance
            // values. The entry names are captured at open time, while the
            // cursor lives in the instance's shared attribute cell so aliases
            // to the handle observe the same iteration state.
            // Cost: O(d) + syscalls, d = entries in the directory (all read at open).
            "opendir" => {
                let path = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_buf = self.resolve_path(&path);
                let entries = match fs::read_dir(&path_buf) {
                    Ok(entries) => entries,
                    Err(_) => {
                        return Some(Err(RuntimeError::new(format!(
                            "Failed to open directory: {path}"
                        ))));
                    }
                };
                let mut names = Vec::new();
                for entry in entries {
                    let entry = match entry {
                        Ok(entry) => entry,
                        Err(_) => {
                            return Some(Err(RuntimeError::new(format!(
                                "Failed to read directory: {path}"
                            ))));
                        }
                    };
                    names.push(Value::str(entry.file_name().to_string_lossy().into_owned()));
                }
                let mut attrs = crate::value::AttrMap::new();
                attrs.insert("entries", Value::array(names));
                attrs.insert("index", Value::int(0));
                attrs.insert("closed", Value::FALSE);
                Ok(Value::make_instance_without_destroy(
                    crate::symbol::Symbol::intern("__NQPDirHandle"),
                    attrs,
                ))
            }
            // Cost: O(1).
            "nextfiledir" => {
                let handle = args.first().cloned().unwrap_or(Value::NIL);
                let crate::value::ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = handle.view()
                else {
                    return Some(Err(RuntimeError::new(
                        "nqp::nextfiledir: expected a directory handle",
                    )));
                };
                if class_name.resolve() != "__NQPDirHandle" {
                    return Some(Err(RuntimeError::new(
                        "nqp::nextfiledir: expected a directory handle",
                    )));
                }
                let (entries, index, closed) = {
                    let attrs = attributes.as_map();
                    (
                        attrs.get("entries").cloned().unwrap_or(Value::NIL),
                        attrs
                            .get("index")
                            .map(crate::runtime::to_int)
                            .unwrap_or(0)
                            .max(0) as usize,
                        attrs.get("closed").is_some_and(Value::truthy),
                    )
                };
                if closed {
                    return Some(Ok(Value::str_from("")));
                }
                let Some(name) = (match entries.view() {
                    crate::value::ValueView::Array(items, _) => items.get(index).cloned(),
                    _ => None,
                }) else {
                    return Some(Ok(Value::str_from("")));
                };
                attributes.insert("index", Value::int((index + 1) as i64));
                Ok(name)
            }
            // Cost: O(1).
            "closedir" => {
                let handle = args.first().cloned().unwrap_or(Value::NIL);
                if let crate::value::ValueView::Instance { attributes, .. } = handle.view() {
                    attributes.insert("closed", Value::TRUE);
                }
                Ok(Value::NIL)
            }
            // Cost: O(p) + one syscall, p = length of $path.
            "fileislink" => {
                let path = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_buf = self.resolve_path(&path);
                Ok(Value::int(i64::from(
                    fs::symlink_metadata(path_buf)
                        .map(|metadata| metadata.file_type().is_symlink())
                        .unwrap_or(false),
                )))
            }
            // Cost: O(p) + syscalls, p = length of $path.
            "filereadable" => {
                let path = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_buf = self.resolve_path(&path);
                Ok(Value::int(i64::from(
                    fs::metadata(&path_buf)
                        .map(|_| path_is_readable(&path_buf))
                        .unwrap_or(false),
                )))
            }

            // -- type test --
            // `istype_nd` is the no-decontainerize sibling of `istype`; mutsu
            // already decontainerizes every operand once at this function's
            // boundary (see the comment above), so the two ops observe the
            // same value here and share one implementation.
            // Cost: O(d), d = depth of the value's MRO/role closure (full type check,
            // no type-check cache). MoarVM: O(1) amortized (type-check cache) -- see #9134.
            "istype" | "istype_nd" => {
                // Operands are already decontainerized at the `call_nqp_op`
                // boundary, so a promoted element container answers about what
                // it holds.
                let v = args.first().cloned().unwrap_or(Value::NIL);
                let type_name = match args.get(1).map(|t| t.view()) {
                    Some(ValueView::Package(p)) => p.resolve(),
                    Some(ValueView::Instance { class_name, .. }) => class_name.resolve(),
                    // `Nil` used as a type argument (`nqp::istype($x, Nil)`) is a
                    // bare `ValueView::Nil`, not a `Package("Nil")` type object
                    // like other builtin types — CBOR::Simple's absent-value
                    // encoding (`nqp::istype($_, Nil)` on an array element bound
                    // to Nil via BIND-POS) hit exactly this gap, always False.
                    Some(ValueView::Nil) => "Nil".to_string(),
                    // A role's pun (`R.^pun`) — and `R.^pun.WHAT`, which the
                    // real `Test.rakumod`'s `isa-ok` calls when its expected
                    // type isn't a `Str:D` (`nqp::istype($var, $type.WHAT)`)
                    // — is a `Mixin`-wrapped `Package`/`Instance` (see
                    // `punned_role_type_object`), not a bare `Package`.
                    // Unwrap it so a pun used as a type argument here behaves
                    // like the class it puns to, exactly as the `isa`/`~~`
                    // fixes for the same shape do.
                    Some(ValueView::Mixin(inner, _)) => match inner.view() {
                        ValueView::Package(p) => p.resolve(),
                        ValueView::Instance { class_name, .. } => class_name.resolve(),
                        _ => String::new(),
                    },
                    _ => String::new(),
                };
                Ok(bool_int(
                    !type_name.is_empty() && self.type_matches_value(&type_name, &v),
                ))
            }

            // -- boxing --
            // Cost: O(n), n = chars of $s (copied). MoarVM: O(1) -- see #9134.
            "p6box_s" => Ok(Value::str(
                args.first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            )),
            // Cost: O(1).
            "p6box_i" => Ok(Value::int(iarg(args, 0))),
            // Cost: O(1).
            "p6box_n" => Ok(Value::num(narg(args, 0))),
            // nqp::unbox_s($x): the native str inside a boxed `Str`. mutsu
            // has no separate native-str representation, so this is the
            // value's string form -- the boxing/unboxing pair `p6box_s` /
            // `unbox_s` round-trips through the same string either way.
            // `Net::Netmask::Fast`'s constructors unbox their `Str:D`
            // parameters before parsing them.
            // Cost: O(n), n = chars of $s (copied). MoarVM: O(1) -- see #9134.
            "unbox_s" => Ok(Value::str(
                args.first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            )),
            // nqp::coerce_is($i): a native int coerced to its decimal string,
            // as `nqp::coerce_in`/`nqp::coerce_ni`/... are for num<->int.
            // `Net::Netmask::Fast` stringifies netmask bit counts this way.
            // Cost: O(1).
            "coerce_is" => Ok(Value::str(iarg(args, 0).to_string())),
            // nqp::coerce_si($s): the inverse -- a native str parsed as a
            // leading-integer prefix (`strtol` style: skip leading
            // whitespace, an optional sign, then digits; no digits at all
            // parses as 0; an out-of-i64-range magnitude saturates rather
            // than erroring). `Net::Netmask::Fast` parses CIDR bit counts and
            // octet strings this way.
            // Cost: O(n), n = chars of $s (copied, then parsed).
            "coerce_si" => Ok(Value::int(parse_leading_int(
                &args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            ))),

            // nqp::objprimspec($type): the REPR primitive storage a type
            // object was declared with -- 0 for an ordinary (boxed) type,
            // 1 for a native (u)int family member, 2 for `num`, 3 for `str`.
            // Rakudo additionally answers 10 for the *unsigned* int family
            // (`uint`/`uint8`/.../`byte`) rather than folding it into 1 --
            // verified against `nqp::objprimspec(uint32)` under rakudo.
            // `AttrX::Mooish`'s `is mooish` trait handler uses this to reject
            // attributes declared with a native type (`nqp::objprimspec($attr.type)`).
            // Cost: O(1).
            "objprimspec" => {
                let v = args.first().cloned().unwrap_or(Value::NIL);
                let type_name = match v.view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => String::new(),
                };
                let spec = match crate::runtime::native_types::native_family(&type_name) {
                    Some("int") => 1,
                    Some("uint") => 10,
                    Some("num") => 2,
                    Some("str") => 3,
                    _ => 0,
                };
                Ok(Value::int(spec))
            }

            // -- string / aggregate queries --
            // Graphemes, as `.chars` (the same routine, `str_prim::chars`).
            // Cost: O(1) amortized: a long `Str` answers from its cached
            // index (built in O(n) on first use, `grapheme_index`).
            "chars" => Ok(Value::int(
                args.first()
                    .map(|v| crate::builtins::str_prim::chars(v) as i64)
                    .unwrap_or(0),
            )),
            // Cost: O(1).
            "elems" => {
                let v = args.first().cloned().unwrap_or(Value::NIL);
                let n = match v.view() {
                    ValueView::Array(items, _) => items.len() as i64,
                    ValueView::Hash(map) => map.len() as i64,
                    // An `IterationBuffer` keeps its elements in an attribute
                    // rather than a buf payload, and a `Uni` answers with its
                    // codepoints, so ask the shared accessor before falling
                    // back to the buf length.
                    _ => Self::nqp_elems_len_of(&v)
                        .or_else(|| match v.view() {
                            ValueView::Instance { attributes, .. } => {
                                value_buf::buf_len(&attributes)
                            }
                            _ => None,
                        })
                        .unwrap_or(0) as i64,
                };
                Ok(Value::int(n))
            }

            // -- byte-string decode (nqp::decode(buf, 'utf8') -> str) --
            // Cost: O(n), n = bytes of $buf (decoded into a fresh string).
            // The one builtin decoder `Blob.decode` uses (ADR-0118 §2.4): the
            // same encoding names, strict ASCII, BOM handling, error text and
            // NFC normalization. This used to be a private decoder that let
            // bytes > 127 through as "ascii" and kept a UTF-8 BOM.
            "decode" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let enc = args
                    .get(1)
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "utf8".to_string());
                match with_buf_bytes_of(op, &buf, |bytes| {
                    crate::builtins::decode_bytes_with_encoding_label(bytes, &enc).unwrap_or_else(
                        || {
                            Err(RuntimeError::new(format!(
                                "Unknown string encoding: '{enc}'"
                            )))
                        },
                    )
                }) {
                    Err(e) | Ok(Err(e)) => Err(e),
                    Ok(Ok(s)) => Ok(Value::str(s)),
                }
            }

            // -- positional element access (buf bytes or array elements) --
            // Cost: O(1) on a Buf of any width (the element is decoded at the buffer's
            // own width) and on an array. MoarVM: O(1).
            "atpos_i" | "atpos_n" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let idx = iarg(args, 1);
                // A Buf/Blob answers from its storage, one element decoded at
                // the buffer's own width and signedness; everything else (a
                // plain array, an IterationBuffer, a Uni's codepoints) from the
                // shared element accessor.
                let elem = match target.view() {
                    ValueView::Instance { attributes, .. }
                        if value_buf::buf_len(&attributes).is_some() =>
                    {
                        usize::try_from(idx)
                            .ok()
                            .and_then(|i| value_buf::buf_elem_at(&attributes, i))
                    }
                    _ => usize::try_from(idx)
                        .ok()
                        .and_then(|i| Self::nqp_elem_at(&target, i)),
                };
                let elem = elem.unwrap_or(Value::int(0));
                if op == "atpos_n" {
                    Ok(Value::num(elem.to_f64()))
                } else {
                    Ok(Value::int(crate::runtime::to_int(&elem)))
                }
            }
            // Cost: O(1) amortized on an array and on a Buf (one element encoded in place);
            // O(i - e) when growing, i = index, e = elements.
            "bindpos_i" | "bindpos_n" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let idx = iarg(args, 1).max(0) as usize;
                let val = args.get(2).cloned().unwrap_or(Value::int(0));
                match target.view() {
                    ValueView::Instance { attributes, .. } => {
                        if value_buf::set_buf_elem(&attributes, idx, &val).is_none() {
                            return Some(Err(RuntimeError::new(format!(
                                "nqp::{op}: expected a Buf/Blob or array"
                            ))));
                        }
                        Ok(val)
                    }
                    ValueView::Array(items, _) => {
                        // SAFETY: audited aliased in-place container write (see
                        // value::aliased_mut) — same pattern as deepmap's
                        // element writeback; no borrow into the node is live.
                        let data = unsafe { crate::value::gc_contents_mut(&items) };
                        if data.items().len() <= idx {
                            data.items_mut().resize(idx + 1, Value::int(0));
                        }
                        data.items_mut()[idx] = val.clone();
                        Ok(val)
                    }
                    _ => Err(RuntimeError::new(format!(
                        "nqp::{op}: expected a Buf/Blob or array"
                    ))),
                }
            }

            // -- slice / splice (buf) --
            // nqp::slice($buf, $start, $end) — END-INCLUSIVE, same class out.
            // Cost: O(k), k = bytes sliced (the source is borrowed, not copied).
            "slice" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let start = iarg(args, 1).max(0) as usize;
                let end = iarg(args, 2);
                let piece = with_buf_bytes_of(op, &buf, |bytes| {
                    let end = if end < 0 {
                        (bytes.len() as i64 + end).max(0) as usize
                    } else {
                        end as usize
                    };
                    let upper = end.saturating_add(1).min(bytes.len());
                    if start < upper {
                        bytes[start..upper].to_vec()
                    } else {
                        Vec::new()
                    }
                });
                match (buf.view(), piece) {
                    (ValueView::Instance { class_name, .. }, Ok(piece)) => {
                        Ok(value_buf::make_buf_from_bytes(class_name, &piece))
                    }
                    (_, Err(e)) => Err(e),
                    _ => Err(RuntimeError::new(
                        "nqp::slice: expected a Buf/Blob".to_string(),
                    )),
                }
            }
            // nqp::splice($target, $source, $offset, $count) — replace
            // target[offset .. offset+count) with source's elements, in place.
            // Cost: O(s + t), s = source elems, t = target elems after $offset (a Buf target is
            // spliced in place; a width-1 source is copied once).
            "splice" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let source = args.get(1).cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 2).max(0) as usize;
                let count = iarg(args, 3).max(0) as usize;
                // An element-store target (a plain list, an IterationBuffer, a
                // Uni) splices VALUES; only a Buf/Blob splices bytes. nqp code
                // builds escaped text by splicing one native int list into
                // another, which the byte path cannot express.
                if let Some(r) = Self::nqp_splice_elems(op, &target, &source, offset, count) {
                    return Some(r);
                }
                let src_bytes = match with_buf_bytes_of(op, &source, <[u8]>::to_vec) {
                    Ok(b) => b,
                    Err(e) => return Some(Err(e)),
                };
                let r = buf_bytes_mutate(op, &target, |bytes| {
                    if bytes.len() < offset {
                        bytes.resize(offset, 0);
                    }
                    let upper = (offset + count).min(bytes.len());
                    bytes.splice(offset..upper, src_bytes.iter().copied());
                    Ok(())
                });
                match r {
                    Ok(()) => Ok(target),
                    Err(e) => Err(e),
                }
            }

            // -- sized binary reads/writes --
            // Cost: O(1) on a width-1 buffer (read straight off the storage); O(e) on a
            // wider one, e = elements (projected to low bytes first). MoarVM: O(1) -- see #9191.
            "readuint" | "readint" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 1).max(0) as usize;
                let (size, endian) = flag_size_endian(iarg(args, 2));
                match with_buf_bytes_of(op, &buf, |bytes| {
                    if bytes.len() < offset.saturating_add(size) {
                        Err(RuntimeError::new(mvm_array_read_buf_oob_message(
                            offset,
                            bytes.len(),
                            size,
                        )))
                    } else {
                        Ok(crate::builtins::read_int_value(
                            &bytes[offset..],
                            size,
                            op == "readint",
                            endian,
                        ))
                    }
                }) {
                    Err(e) | Ok(Err(e)) => Err(e),
                    Ok(Ok(v)) => Ok(v),
                }
            }
            // Cost: O(1) on a width-1 buffer (read straight off the storage); O(e) on a
            // wider one, e = elements (projected to low bytes first). MoarVM: O(1) -- see #9191.
            "readnum" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 1).max(0) as usize;
                let (size, endian) = flag_size_endian(iarg(args, 2));
                match with_buf_bytes_of(op, &buf, |bytes| {
                    if bytes.len() < offset.saturating_add(size) {
                        Err(RuntimeError::new(mvm_array_read_buf_oob_message(
                            offset,
                            bytes.len(),
                            size,
                        )))
                    } else if size == 4 {
                        Ok(Value::num(crate::builtins::read_f32_endian(
                            &bytes[offset..],
                            endian,
                        )))
                    } else {
                        Ok(Value::num(crate::builtins::read_f64_endian(
                            &bytes[offset..],
                            endian,
                        )))
                    }
                }) {
                    Err(e) | Ok(Err(e)) => Err(e),
                    Ok(Ok(v)) => Ok(v),
                }
            }
            // Cost: O(1) amortized on a width-1 buffer (bytes written in place); O(e) on a
            // wider one, e = elements (re-encoded whole). MoarVM: O(1) amortized -- see #9191.
            "writeuint" | "writeint" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 1);
                let val = args.get(2).cloned().unwrap_or(Value::int(0));
                let (size, endian) = flag_size_endian(iarg(args, 3));
                let method = write_method_for(size, op == "writeint");
                let r = buf_bytes_mutate(op, &buf, |bytes| {
                    // `buf_bytes_mutate` hands over one byte per element, so the
                    // offset is already a plain byte offset — width 1.
                    crate::builtins::buf_write_int::apply_write_int(
                        bytes, method, offset, &val, endian, 1,
                    )
                });
                match r {
                    Ok(()) => Ok(val),
                    Err(e) => Err(e),
                }
            }
            // Cost: O(1) amortized on a width-1 buffer (bytes written in place); O(e) on a
            // wider one, e = elements (re-encoded whole). MoarVM: O(1) amortized -- see #9191.
            "writenum" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 1).max(0) as usize;
                let n = narg(args, 2);
                let (size, endian) = flag_size_endian(iarg(args, 3));
                let r = buf_bytes_mutate(op, &buf, |bytes| {
                    let needed = offset + size;
                    if bytes.len() < needed {
                        bytes.resize(needed, 0);
                    }
                    if size == 4 {
                        let enc = match endian {
                            1 => (n as f32).to_le_bytes(),
                            2 => (n as f32).to_be_bytes(),
                            _ => (n as f32).to_ne_bytes(),
                        };
                        bytes[offset..offset + 4].copy_from_slice(&enc);
                    } else {
                        let enc = match endian {
                            1 => n.to_le_bytes(),
                            2 => n.to_be_bytes(),
                            _ => n.to_ne_bytes(),
                        };
                        bytes[offset..offset + 8].copy_from_slice(&enc);
                    }
                    Ok(())
                });
                match r {
                    Ok(()) => Ok(Value::num(n)),
                    Err(e) => Err(e),
                }
            }

            // -- low-level file handles --
            // nqp::open($path, $mode) -> byte-oriented handle. Mode letters
            // follow MoarVM: 'r' read, 'w' write (create+truncate), 'wa'
            // append (create), 'x' exclusive create. Always binary; nqp
            // string reads go through explicit decode ops, not the handle.
            // Driver: Crypt::Random reads /dev/urandom this way.
            // Cost: O(p) + syscalls, p = length of $path.
            "open" => {
                let path = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mode = args
                    .get(1)
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "r".to_string());
                let (read, write, append, create, exclusive) = match mode.as_str() {
                    "r" => (true, false, false, false, false),
                    "w" => (false, true, false, true, false),
                    "wa" => (false, true, true, true, false),
                    "x" => (false, true, false, true, true),
                    other => {
                        return Some(Err(RuntimeError::new(format!(
                            "nqp::open: unknown mode '{other}'"
                        ))));
                    }
                };
                let path_buf = self.resolve_path(&path);
                self.open_file_handle(
                    &path_buf,
                    read,
                    write,
                    append,
                    true,
                    false,
                    Vec::new(),
                    None,
                    None,
                    None,
                    create,
                    exclusive,
                    None,
                )
            }
            // nqp::readfh($fh, $buf, $count) — read up to $count bytes,
            // REPLACING the buffer's contents (MoarVM semantics), and return
            // the buffer. A short read (EOF) is not an error.
            // Cost: O(c) + syscalls, c = bytes read (a width-1 $buf is overwritten in place).
            "readfh" => {
                let fh = args.first().cloned().unwrap_or(Value::NIL);
                let buf = args.get(1).cloned().unwrap_or(Value::NIL);
                let count = iarg(args, 2).max(0) as usize;
                let bytes = match self.read_bytes_from_handle_value(&fh, count) {
                    Ok(b) => b,
                    Err(e) => return Some(Err(e)),
                };
                let r = buf_bytes_mutate(op, &buf, |dst| {
                    *dst = bytes;
                    Ok(())
                });
                match r {
                    Ok(()) => Ok(buf),
                    Err(e) => Err(e),
                }
            }
            // nqp::closefh($fh) — close and return the handle.
            // Cost: O(1) + syscall.
            "closefh" => {
                let fh = args.first().cloned().unwrap_or(Value::NIL);
                match self.close_handle_value(&fh) {
                    Ok(_) => Ok(fh),
                    Err(e) => Err(e),
                }
            }

            // The process/introspection half of the table lives in its own
            // module (file-size limit); an op neither knows still errors.
            _ => return self.call_nqp_op_process(op, args),
        })
    }
}
