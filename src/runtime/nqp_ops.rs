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

use crate::runtime::{Interpreter, RuntimeError};
use crate::value::value_buf;
use crate::value::{Value, ValueView};

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn narg(args: &[Value], i: usize) -> f64 {
    args.get(i).map(|v| v.to_f64()).unwrap_or(0.0)
}

fn bool_int(b: bool) -> Value {
    Value::int(i64::from(b))
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

/// The bytes of a Buf/Blob instance, or an error naming the op.
fn buf_bytes_of(op: &str, v: &Value) -> Result<Vec<u8>, RuntimeError> {
    if let ValueView::Instance { attributes, .. } = v.view()
        && let Some(bytes) = value_buf::buf_bytes(&attributes)
    {
        return Ok(bytes);
    }
    Err(RuntimeError::new(format!(
        "nqp::{op}: expected a Buf/Blob, got {}",
        crate::runtime::value_type_name(v)
    )))
}

/// Mutate a Buf instance's bytes in place through its shared attribute cell
/// (alias-visible), or error naming the op.
fn buf_bytes_mutate(
    op: &str,
    v: &Value,
    f: impl FnOnce(&mut Vec<u8>) -> Result<(), RuntimeError>,
) -> Result<(), RuntimeError> {
    if let ValueView::Instance { attributes, .. } = v.view() {
        let done = value_buf::with_buf_elems_mut(&attributes, |elems| {
            let mut bytes: Vec<u8> = elems
                .iter()
                .map(|e| crate::runtime::to_int(e) as u8)
                .collect();
            let r = f(&mut bytes);
            *elems = value_buf::bytes_to_elems(&bytes);
            r
        });
        if let Some(r) = done {
            return r;
        }
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
        Some(match op {
            // -- native int arithmetic / bit ops --
            "add_i" => Ok(Value::int(iarg(args, 0).wrapping_add(iarg(args, 1)))),
            "sub_i" => Ok(Value::int(iarg(args, 0).wrapping_sub(iarg(args, 1)))),
            "mul_i" => Ok(Value::int(iarg(args, 0).wrapping_mul(iarg(args, 1)))),
            "neg_i" => Ok(Value::int(iarg(args, 0).wrapping_neg())),
            "abs_i" => Ok(Value::int(iarg(args, 0).wrapping_abs())),
            "bitor_i" => Ok(Value::int(iarg(args, 0) | iarg(args, 1))),
            "bitand_i" => Ok(Value::int(iarg(args, 0) & iarg(args, 1))),
            "bitxor_i" => Ok(Value::int(iarg(args, 0) ^ iarg(args, 1))),
            "bitneg_i" => Ok(Value::int(!iarg(args, 0))),
            "bitshiftl_i" => Ok(Value::int(
                iarg(args, 0).wrapping_shl(iarg(args, 1).clamp(0, 63) as u32),
            )),
            "bitshiftr_i" => Ok(Value::int(
                iarg(args, 0).wrapping_shr(iarg(args, 1).clamp(0, 63) as u32),
            )),
            // Arbitrary-precision add: nqp::add_I($a, $b, Int) — the third
            // argument is the boxing target type and is ignored here.
            "add_I" => Ok(Value::from_bigint(
                args.first().map(|v| v.to_bigint()).unwrap_or_default()
                    + args.get(1).map(|v| v.to_bigint()).unwrap_or_default(),
            )),
            "sub_I" => Ok(Value::from_bigint(
                args.first().map(|v| v.to_bigint()).unwrap_or_default()
                    - args.get(1).map(|v| v.to_bigint()).unwrap_or_default(),
            )),

            // -- native int comparisons (yield int 0/1, as in nqp) --
            "iseq_i" => Ok(bool_int(iarg(args, 0) == iarg(args, 1))),
            "isne_i" => Ok(bool_int(iarg(args, 0) != iarg(args, 1))),
            "islt_i" => Ok(bool_int(iarg(args, 0) < iarg(args, 1))),
            "isle_i" => Ok(bool_int(iarg(args, 0) <= iarg(args, 1))),
            "isgt_i" => Ok(bool_int(iarg(args, 0) > iarg(args, 1))),
            "isge_i" => Ok(bool_int(iarg(args, 0) >= iarg(args, 1))),
            "not_i" => Ok(bool_int(iarg(args, 0) == 0)),

            // -- native num comparisons --
            "iseq_n" => Ok(bool_int(narg(args, 0) == narg(args, 1))),
            "isne_n" => Ok(bool_int(narg(args, 0) != narg(args, 1))),
            "islt_n" => Ok(bool_int(narg(args, 0) < narg(args, 1))),
            "isle_n" => Ok(bool_int(narg(args, 0) <= narg(args, 1))),
            "isgt_n" => Ok(bool_int(narg(args, 0) > narg(args, 1))),
            "isge_n" => Ok(bool_int(narg(args, 0) >= narg(args, 1))),
            "isnanorinf" => Ok(bool_int({
                let n = narg(args, 0);
                n.is_nan() || n.is_infinite()
            })),

            // -- type test --
            "istype" => {
                let v = args.first().cloned().unwrap_or(Value::NIL);
                let type_name = match args.get(1).map(|t| t.view()) {
                    Some(ValueView::Package(p)) => p.resolve().to_string(),
                    Some(ValueView::Instance { class_name, .. }) => {
                        class_name.resolve().to_string()
                    }
                    _ => String::new(),
                };
                Ok(bool_int(
                    !type_name.is_empty() && self.type_matches_value(&type_name, &v),
                ))
            }

            // -- boxing --
            "p6box_s" => Ok(Value::str(
                args.first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            )),
            "p6box_i" => Ok(Value::int(iarg(args, 0))),
            "p6box_n" => Ok(Value::num(narg(args, 0))),

            // -- string / aggregate queries --
            "chars" => Ok(Value::int(
                args.first()
                    .map(|v| v.to_string_value().chars().count() as i64)
                    .unwrap_or(0),
            )),
            "elems" => {
                let v = args.first().cloned().unwrap_or(Value::NIL);
                let n = match v.view() {
                    ValueView::Instance { attributes, .. } => {
                        value_buf::buf_len(&attributes).unwrap_or(0) as i64
                    }
                    ValueView::Array(items, _) => items.len() as i64,
                    ValueView::Hash(map) => map.len() as i64,
                    _ => 0,
                };
                Ok(Value::int(n))
            }

            // -- byte-string decode (nqp::decode(buf, 'utf8') -> str) --
            "decode" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let enc = args
                    .get(1)
                    .map(|v| v.to_string_value().to_lowercase())
                    .unwrap_or_else(|| "utf8".to_string());
                match buf_bytes_of(op, &buf) {
                    Err(e) => Err(e),
                    Ok(bytes) => match enc.as_str() {
                        "utf8" | "utf-8" => match String::from_utf8(bytes) {
                            Ok(s) => Ok(Value::str(s)),
                            Err(_) => Err(RuntimeError::new(
                                "Malformed UTF-8 in nqp::decode".to_string(),
                            )),
                        },
                        "ascii" | "latin-1" | "iso-8859-1" => Ok(Value::str(
                            bytes.iter().map(|&b| b as char).collect::<String>(),
                        )),
                        other => Err(RuntimeError::new(format!(
                            "nqp::decode: unsupported encoding '{other}'"
                        ))),
                    },
                }
            }

            // -- positional element access (buf bytes or array elements) --
            "atpos_i" | "atpos_n" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let idx = iarg(args, 1);
                let elem = match target.view() {
                    ValueView::Instance { attributes, .. } => usize::try_from(idx)
                        .ok()
                        .and_then(|i| {
                            value_buf::with_buf_bytes(&attributes, |b| b.get(i).copied()).flatten()
                        })
                        .map(|b| Value::int(b as i64)),
                    ValueView::Array(items, _) => usize::try_from(idx)
                        .ok()
                        .and_then(|i| items.get(i).cloned()),
                    _ => None,
                };
                let elem = elem.unwrap_or(Value::int(0));
                if op == "atpos_n" {
                    Ok(Value::num(elem.to_f64()))
                } else {
                    Ok(Value::int(crate::runtime::to_int(&elem)))
                }
            }
            "bindpos_i" | "bindpos_n" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let idx = iarg(args, 1).max(0) as usize;
                let val = args.get(2).cloned().unwrap_or(Value::int(0));
                match target.view() {
                    ValueView::Instance { attributes, .. } => {
                        let stored = val.clone();
                        let done = value_buf::with_buf_elems_mut(&attributes, |elems| {
                            if elems.len() <= idx {
                                elems.resize(idx + 1, Value::int(0));
                            }
                            elems[idx] = stored;
                        });
                        if done.is_none() {
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
                        if data.items.len() <= idx {
                            data.items.resize(idx + 1, Value::int(0));
                        }
                        data.items[idx] = val.clone();
                        Ok(val)
                    }
                    _ => Err(RuntimeError::new(format!(
                        "nqp::{op}: expected a Buf/Blob or array"
                    ))),
                }
            }

            // -- slice / splice (buf) --
            // nqp::slice($buf, $start, $end) — END-INCLUSIVE, same class out.
            "slice" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let start = iarg(args, 1).max(0) as usize;
                let end = iarg(args, 2);
                match (buf.view(), buf_bytes_of(op, &buf)) {
                    (ValueView::Instance { class_name, .. }, Ok(bytes)) => {
                        let end = if end < 0 {
                            (bytes.len() as i64 + end).max(0) as usize
                        } else {
                            end as usize
                        };
                        let upper = (end + 1).min(bytes.len());
                        let piece = if start < upper {
                            bytes[start..upper].to_vec()
                        } else {
                            Vec::new()
                        };
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
            "splice" => {
                let target = args.first().cloned().unwrap_or(Value::NIL);
                let source = args.get(1).cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 2).max(0) as usize;
                let count = iarg(args, 3).max(0) as usize;
                let src_bytes = match buf_bytes_of(op, &source) {
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
            "readuint" | "readint" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 1).max(0) as usize;
                let (size, endian) = flag_size_endian(iarg(args, 2));
                match buf_bytes_of(op, &buf) {
                    Err(e) => Err(e),
                    Ok(bytes) => {
                        if bytes.len() < offset + size {
                            Err(RuntimeError::new(format!(
                                "nqp::{op}: read of {size} bytes at offset {offset} past end ({} bytes)",
                                bytes.len()
                            )))
                        } else {
                            Ok(crate::builtins::read_int_value(
                                &bytes[offset..],
                                size,
                                op == "readint",
                                endian,
                            ))
                        }
                    }
                }
            }
            "readnum" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 1).max(0) as usize;
                let (size, endian) = flag_size_endian(iarg(args, 2));
                match buf_bytes_of(op, &buf) {
                    Err(e) => Err(e),
                    Ok(bytes) => {
                        if bytes.len() < offset + size {
                            Err(RuntimeError::new(format!(
                                "nqp::readnum: read of {size} bytes at offset {offset} past end"
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
                    }
                }
            }
            "writeuint" | "writeint" => {
                let buf = args.first().cloned().unwrap_or(Value::NIL);
                let offset = iarg(args, 1);
                let val = args.get(2).cloned().unwrap_or(Value::int(0));
                let (size, endian) = flag_size_endian(iarg(args, 3));
                let method = write_method_for(size, op == "writeint");
                let r = buf_bytes_mutate(op, &buf, |bytes| {
                    crate::builtins::buf_write_int::apply_write_int(
                        bytes, method, offset, &val, endian,
                    )
                });
                match r {
                    Ok(()) => Ok(val),
                    Err(e) => Err(e),
                }
            }
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

            _ => return None,
        })
    }
}
