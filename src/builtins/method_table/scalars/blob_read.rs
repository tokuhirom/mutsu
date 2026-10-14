//! `Blob`/`Buf`'s methods that take arguments: the `read-*` accessors,
//! `read-ubits`/`read-bits` and `subbuf` (ADR-11276 §8.3, §9.107).
//!
//! Each `read-*` method is one function over (what it reads, receiver, offset,
//! endianness); the per-name function pointers a row needs are generated from
//! one table that also says what each name reads. The arguments bind as
//! Rakudo's signatures do (`int $offset`, `Endian $endian`, `uint $bits`), so
//! the rows answer every call: no cascade arm is left behind them.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::NarrowFn;
use crate::builtins::methods_narg::buf::{
    buf_class_name, buf_get_int_items, buf_get_raw_bytes, is_buf_like, make_buf_from_int_items,
    read_byte_offset, read_f32_endian, read_f32_ne, read_f64_endian, read_f64_ne, read_int_value,
};
use crate::value::{RuntimeError, Value, ValueView};

/// What a `read-*` method reads: an integer of `size` bytes, or a float.
#[derive(Clone, Copy)]
enum Reads {
    Int { size: usize, signed: bool },
    Num { size: usize },
}

/// An argument bound to a native `int` parameter, as Rakudo unboxes it: an
/// `Int` (a `Bool` or an `Int`-valued enum is one), and an error for a type
/// object, a big integer and anything else.
// Cost: O(1).
fn native_int(arg: &Value) -> Result<i64, RuntimeError> {
    match arg.view() {
        ValueView::Int(i) => Ok(i),
        ValueView::Bool(b) => Ok(i64::from(b)),
        ValueView::Enum {
            value: crate::value::EnumValue::Int(i),
            ..
        } => Ok(*i),
        ValueView::BigInt(n) => Err(RuntimeError::new(format!(
            "Cannot unbox {} bit wide bigint into native integer. Did you mix int and Int or literals?",
            n.bits()
        ))),
        ValueView::Package(name) => Err(RuntimeError::new(format!(
            "Cannot unbox a type object ({}) to int.",
            crate::value::user_facing_type_name(name.as_str())
        ))),
        _ => Err(RuntimeError::new(format!(
            "This type cannot unbox to a native integer: P6opaque, {}",
            crate::value::types::what_type_name(arg)
        ))),
    }
}

/// The endianness argument of a two-argument `read-*`, bound to `Endian
/// $endian`: an `Endian` enum value, and a type-check failure for anything
/// else (an `Int` too).
// Cost: O(1).
fn endian_of(arg: &Value) -> Result<i64, RuntimeError> {
    if let ValueView::Enum {
        enum_type, value, ..
    } = arg.view()
        && enum_type == "Endian"
    {
        return Ok(value.as_i64());
    }
    let got = crate::value::types::what_type_name(arg);
    let message = format!(
        "Type check failed in binding to parameter '$endian'; expected Endian but got {got} ({})",
        crate::value::raku_repr::raku_value(arg),
    );
    Err(RuntimeError::typecheck_binding_parameter(
        "$endian",
        "Endian",
        &got,
        Some(message),
    ))
}

/// `read-uint8`, `read-int16`, `read-num32` and the rest: the number stored at
/// element `args[0]` of the buffer, in the byte order `args[1]` names (native
/// when it is absent).
// Cost: O(e), e = elements (the bytes are copied out of the storage once).
fn read(reads: Reads, target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (bytes, width) = buf_get_raw_bytes(target)?;
    let offset = match native_int(args.first()?) {
        Ok(offset) => offset,
        Err(e) => return Some(Err(e)),
    };
    let endian = match args.get(1).map(endian_of).transpose() {
        Ok(endian) => endian,
        Err(e) => return Some(Err(e)),
    };
    let size = match reads {
        Reads::Int { size, .. } | Reads::Num { size } => size,
    };
    let offset = match read_byte_offset(&bytes, offset, size, width) {
        Ok(off) => off,
        Err(e) => return Some(Err(e)),
    };
    let window = &bytes[offset..offset + size];
    Some(Ok(match reads {
        Reads::Num { size } => Value::num(match (size, endian) {
            (4, None) => read_f32_ne(window),
            (4, Some(e)) => read_f32_endian(window, e),
            (_, None) => read_f64_ne(window),
            (_, Some(e)) => read_f64_endian(window, e),
        }),
        Reads::Int { size, signed } => {
            // One argument: the native order, spelled as the explicit one it is.
            let endian = endian.unwrap_or(if cfg!(target_endian = "little") { 1 } else { 2 });
            read_int_value(window, size, signed, endian)
        }
    }))
}

macro_rules! readers {
    ($($handler:ident => $name:literal : $reads:expr),* $(,)?) => {
        $(
            // Cost: see `read`.
            fn $handler(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                read($reads, target, args)
            }
        )*

        /// `read-*` at one argument (the offset) and at two (offset and
        /// endianness), on both owners.
        pub(super) static READ_ROWS: &[MethodRow] = &[$(
            read_row($name, 1, $handler, "Blob"),
            read_row($name, 2, $handler, "Blob"),
            read_row($name, 1, $handler, "Buf"),
            read_row($name, 2, $handler, "Buf"),
        )*];
    };
}

const fn read_row(
    name: &'static str,
    arity: u8,
    handler: NarrowFn,
    owner: &'static str,
) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity,
        handler: Handler::Narrow(handler),
        // An `Endian` argument is an enum value, not a plain scalar, and a
        // wrong argument is the handler's binding error.
        flags: RowFlags::ANY_ARGS,
        named: &[],
    }
}

const fn int(size: usize, signed: bool) -> Reads {
    Reads::Int { size, signed }
}

readers! {
    read_uint8 => "read-uint8": int(1, false),
    read_int8 => "read-int8": int(1, true),
    read_uint16 => "read-uint16": int(2, false),
    read_int16 => "read-int16": int(2, true),
    read_uint32 => "read-uint32": int(4, false),
    read_int32 => "read-int32": int(4, true),
    read_uint64 => "read-uint64": int(8, false),
    read_int64 => "read-int64": int(8, true),
    read_uint128 => "read-uint128": int(16, false),
    read_int128 => "read-int128": int(16, true),
    read_num32 => "read-num32": Reads::Num { size: 4 },
    read_num64 => "read-num64": Reads::Num { size: 8 },
}

/// `read-ubits($pos, $bits)` and `read-bits($pos, $bits)`: the `$bits` bits
/// from bit `$pos` on, as an unsigned or a two's-complement integer. Rakudo
/// binds `int $pos, uint $bits`: a negative `$bits` wraps to a count no read
/// reaches, which reads nothing (`0`). A read past the end names what can be
/// read, as Rakudo does without the receiver's variable name.
// Cost: O(e + b), e = bytes of the buffer, b = bits read.
fn read_bits(signed: bool, target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (bytes, _) = buf_get_raw_bytes(target)?;
    let (pos, bits) = match (native_int(args.first()?), native_int(args.get(1)?)) {
        (Ok(pos), Ok(bits)) => (pos, bits),
        (Err(e), _) | (_, Err(e)) => return Some(Err(e)),
    };
    let total = bytes.len() as i64 * 8;
    if pos < 0 {
        return Some(Err(RuntimeError::new(format!(
            "Can only read from position 0..{} in buffer, you tried: {pos}",
            total - 1
        ))));
    }
    if pos.saturating_add(bits) > total {
        return Some(Err(RuntimeError::new(format!(
            "Can only read 1..{} bits from position {pos} in buffer, you tried: {bits}",
            total - pos
        ))));
    }
    if bits <= 0 {
        return Some(Ok(Value::int(0)));
    }
    Some(crate::builtins::buf_bits::read_bits(
        &bytes, pos, bits, signed,
    ))
}

// Cost: see `read_bits`.
fn read_ubits(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    read_bits(false, target, args)
}

// Cost: see `read_bits`.
fn read_sbits(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    read_bits(true, target, args)
}

pub(super) static BITS_ROWS: &[MethodRow] = &[
    read_row("read-ubits", 2, read_ubits, "Blob"),
    read_row("read-ubits", 2, read_ubits, "Buf"),
    read_row("read-bits", 2, read_sbits, "Blob"),
    read_row("read-bits", 2, read_sbits, "Buf"),
];

const fn subbuf_row(name: &'static str, arity: u8, owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity,
        // A `Callable` bound is called with the length, so the handler needs
        // the interpreter.
        handler: Handler::Interp(subbuf_in),
        // The first argument may be a `Range` or a `Callable`, and an argument
        // no candidate takes is the handler's `X::Multi::NoMatch`.
        flags: RowFlags::ANY_ARGS,
        named: &[],
    }
}

/// `subbuf` on both owners, and the value of `Buf.subbuf-rw` read as an
/// rvalue (an lvalue `subbuf-rw` is the interpreter's proxy, `Blob` declares
/// none).
pub(super) static SUBBUF_ROWS: &[MethodRow] = &[
    subbuf_row("subbuf", 1, "Blob"),
    subbuf_row("subbuf", 2, "Blob"),
    subbuf_row("subbuf", 1, "Buf"),
    subbuf_row("subbuf", 2, "Buf"),
    subbuf_row("subbuf-rw", 0, "Buf"),
    subbuf_row("subbuf-rw", 1, "Buf"),
    subbuf_row("subbuf-rw", 2, "Buf"),
];

/// [`subbuf`] with each `Callable` bound (`subbuf(*-2)`, `subbuf(1, *-1)`,
/// Rakudo's `&From`/`&End` candidates) called with the length first.
// Cost: O(e + b), e = elements, b = one run of each Callable.
fn subbuf_in(
    interp: &mut crate::runtime::Interpreter,
    target: &Value,
    args: &[Value],
    _named: super::Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if args.is_empty() {
        return subbuf(target, &[Value::int(0)]);
    }
    match interp.resolve_subbuf_callable_args(target, args) {
        Some(resolved) => subbuf(target, &resolved),
        None => subbuf(target, args),
    }
}

/// An `X::OutOfRange` naming the `subbuf` argument `what` (Rakudo's wording,
/// its stray quote included).
// Cost: O(1).
fn subbuf_out_of_range(what: &str, got: i64, len: usize) -> RuntimeError {
    let mut attrs = std::collections::HashMap::new();
    let range = format!("0..{len}");
    let msg = format!("{what} out of range. Is: {got}, should be in {range}");
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("got".to_string(), Value::int(got));
    attrs.insert("range".to_string(), Value::str(range));
    let ex = Value::make_instance(crate::symbol::Symbol::intern("X::OutOfRange"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// The `X::Multi::NoMatch` of a `subbuf` call no candidate takes.
// Cost: O(a), a = arguments.
fn subbuf_no_match(target: &Value, args: &[Value]) -> RuntimeError {
    let smiley = |v: &Value| {
        let what = crate::value::types::what_type_name(v);
        if matches!(v.view(), ValueView::Package(_)) {
            format!("{what}:U")
        } else {
            format!("{what}:D")
        }
    };
    let args: Vec<String> = args.iter().map(smiley).collect();
    crate::builtins::methods_0arg::make_no_match_error(&format!(
        "subbuf({}: {})",
        smiley(target),
        args.join(", ")
    ))
}

/// The length a `subbuf($from, $length)` call asks for, bound as Rakudo's
/// candidates do: an `Int`, a `Whatever` or an infinite `Num` (the rest of
/// the buffer), or another `Numeric` truncated to an `Int`. `None` when no
/// candidate takes it.
// Cost: O(1).
fn subbuf_length(arg: &Value, rest: usize) -> Option<i64> {
    match arg.view() {
        ValueView::Int(n) => Some(n),
        ValueView::Whatever => Some(rest as i64),
        ValueView::Num(f) if f.is_infinite() && f > 0.0 => Some(rest as i64),
        ValueView::Num(f) if f.is_finite() => Some(f.trunc() as i64),
        ValueView::Rat(n, d) if d != 0 => Some(n / d),
        _ => None,
    }
}

/// `subbuf($range)`, `subbuf($from)` and `subbuf($from, $length)`: a buffer of
/// the same class over part of the elements, the `Callable` bounds already
/// called ([`subbuf_in`]).
// Cost: O(e), e = elements (the elements are copied out of the storage).
fn subbuf(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !is_buf_like(target) {
        return None;
    }
    let items = buf_get_int_items(target)?;
    let class = buf_class_name(target);
    let len = items.len();
    let first = args.first()?;
    let (from, want) = match (first.view(), args.get(1)) {
        (ValueView::Int(from), None) => (from, None),
        (ValueView::Int(from), Some(length)) => {
            let rest = len.saturating_sub(from.clamp(0, len as i64) as usize);
            match subbuf_length(length, rest) {
                Some(want) => (from, Some(want)),
                None => return Some(Err(subbuf_no_match(target, args))),
            }
        }
        (_, None) if first.is_range() => {
            let Some((from, to)) = crate::builtins::range_bounds_int::range_int_bounds(first)
                .and_then(|(from, to)| Some((from.as_int()?, to.as_int()?)))
            else {
                return Some(Err(RuntimeError::new(
                    "Must specify a Range with integer bounds to subbuf",
                )));
            };
            // An empty range is an empty buffer, not a negative length.
            (from, Some(to.saturating_sub(from).saturating_add(1).max(0)))
        }
        _ => return Some(Err(subbuf_no_match(target, args))),
    };
    if from < 0 || from as usize > len {
        return Some(Err(subbuf_out_of_range(
            "\"From argument to subbuf",
            from,
            len,
        )));
    }
    let from = from as usize;
    let take = match want {
        None => len - from,
        Some(want) if want < 0 => {
            return Some(Err(subbuf_out_of_range("Len element to subbuf", want, len)));
        }
        Some(want) => (want as usize).min(len - from),
    };
    Some(Ok(make_buf_from_int_items(
        &class,
        &items[from..from + take],
    )))
}
