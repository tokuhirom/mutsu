//! `Blob`/`Buf`'s methods that take arguments: the `read-*` accessors and
//! `subbuf` (ADR-11276 §8.3).
//!
//! Each `read-*` method is one function over (method name, receiver, offset,
//! endianness); the per-name function pointers a row needs are generated, and
//! the cascades call [`read`] for the receivers that have no shape.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_narg::buf::{
    buf_class_name, buf_get_int_items, buf_get_raw_bytes, is_buf_like, make_buf_from_int_items,
    out_of_range_error, range_bounds, read_byte_offset, read_f32_endian, read_f32_ne,
    read_f64_endian, read_f64_ne, read_int_method_info, read_int_value, resolve_buf_index,
    resolve_buf_len, to_int_val,
};
use crate::value::{RuntimeError, Value, ValueView};

/// The endianness argument of a two-argument `read-*`: an `Endian` enum value
/// or an `Int`, and `NativeEndian` (0) for anything else.
// Cost: O(1).
fn endian_of(arg: &Value) -> i64 {
    match arg.view() {
        ValueView::Enum { value, .. } => value.as_i64(),
        ValueView::Int(i) => i,
        _ => 0,
    }
}

/// `read-uint8`, `read-int16`, `read-num32` and the rest: the number stored at
/// element `args[0]` of the buffer, in the byte order `args[1]` names (native
/// when it is absent). `method` is the full method name.
// Cost: O(e), e = elements (the bytes are copied out of the storage once).
pub(crate) fn read(
    method: &str,
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Package(type_name) = target.view() {
        return Some(Err(RuntimeError::new(format!(
            "Cannot resolve caller {}({}:U)",
            method, type_name,
        ))));
    }
    let (bytes, width) = buf_get_raw_bytes(target)?;
    let offset_i64 = to_int_val(args.first()?);
    let endian = args.get(1).map(endian_of);
    let is_num = method.starts_with("read-num");
    let (size, signed) = if is_num {
        (if method == "read-num32" { 4 } else { 8 }, true)
    } else {
        read_int_method_info(method)
    };
    let offset = match read_byte_offset(&bytes, offset_i64, size, width) {
        Ok(off) => off,
        Err(e) => return Some(Err(e)),
    };
    let window = &bytes[offset..offset + size];
    if is_num {
        let number = match (size, endian) {
            (4, None) => read_f32_ne(window),
            (4, Some(e)) => read_f32_endian(window, e),
            (_, None) => read_f64_ne(window),
            (_, Some(e)) => read_f64_endian(window, e),
        };
        return Some(Ok(Value::num(number)));
    }
    // One argument: the native order, spelled as the explicit one it is.
    let endian = endian.unwrap_or(if cfg!(target_endian = "little") { 1 } else { 2 });
    Some(Ok(read_int_value(window, size, signed, endian)))
}

macro_rules! readers {
    ($($handler:ident => $name:literal),* $(,)?) => {
        $(
            // Cost: see `read`.
            fn $handler(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                read($name, target, args)
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
    handler: fn(&Value, &[Value]) -> Option<Result<Value, RuntimeError>>,
    owner: &'static str,
) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity,
        handler: Handler::Narrow(handler),
        // An `Endian` argument is an enum value, not a plain scalar.
        flags: RowFlags::ANY_ARGS,
        named: &[],
    }
}

readers! {
    read_uint8 => "read-uint8",
    read_int8 => "read-int8",
    read_uint16 => "read-uint16",
    read_int16 => "read-int16",
    read_uint32 => "read-uint32",
    read_int32 => "read-int32",
    read_uint64 => "read-uint64",
    read_int64 => "read-int64",
    read_uint128 => "read-uint128",
    read_int128 => "read-int128",
    read_num32 => "read-num32",
    read_num64 => "read-num64",
}

const fn subbuf_row(arity: u8, owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "subbuf",
        arity,
        handler: Handler::Narrow(subbuf),
        // The first argument may be a `Range`.
        flags: RowFlags::ANY_ARGS,
        named: &[],
    }
}

pub(super) static SUBBUF_ROWS: &[MethodRow] = &[
    subbuf_row(1, "Blob"),
    subbuf_row(2, "Blob"),
    subbuf_row(1, "Buf"),
    subbuf_row(2, "Buf"),
];

/// `subbuf($range)`, `subbuf($from)` and `subbuf($from, $length)`: a buffer of
/// the same class over part of the elements. `Whatever` and `Inf` count from
/// the end.
// Cost: O(e), e = elements (the elements are copied out of the storage).
pub(crate) fn subbuf(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !is_buf_like(target) {
        return None;
    }
    let items = buf_get_int_items(target)?;
    let class = buf_class_name(target);
    let len = items.len();
    let first = args.first()?;
    if args.len() == 1 {
        if let Some((start, end)) = range_bounds(first) {
            if end <= start || start >= len as i64 {
                return Some(Ok(make_buf_from_int_items(&class, &[])));
            }
            let from = start.max(0) as usize;
            let to = (end as usize).min(len);
            return Some(Ok(make_buf_from_int_items(&class, &items[from..to])));
        }
        let start = resolve_buf_index(first, len);
        if start < 0 || start as usize > len {
            return Some(Err(out_of_range_error(start, 0, len as i64)));
        }
        return Some(Ok(make_buf_from_int_items(
            &class,
            &items[start as usize..],
        )));
    }
    let start = resolve_buf_index(first, len);
    if start < 0 || start as usize > len {
        return Some(Err(out_of_range_error(start, 0, len as i64)));
    }
    let count = resolve_buf_len(args.get(1)?, len, start as usize);
    if count < 0 {
        return Some(Err(out_of_range_error(count, 0, len as i64)));
    }
    let from = start as usize;
    let take = (count as usize).min(len - from);
    Some(Ok(make_buf_from_int_items(
        &class,
        &items[from..from + take],
    )))
}
