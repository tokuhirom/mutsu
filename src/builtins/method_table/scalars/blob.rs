//! The byte buffers' rows: `Blob`, `Buf` and the encoding buffers (ADR-11276
//! §8.3, slice 3B remainder).
//!
//! Rakudo has no inheritance between them: `Blob`, `Buf` and `utf8` are
//! separate classes, each declaring its own copy of every method (the role
//! `Blob` is composed into all of them), and the recognition table folds the
//! three into one owner, `Blob`. So the read-only rows are registered once
//! per shape's own type, `Blob` and `Buf`, sharing one handler each; the
//! mutators belong to `Buf` alone (`Buf` declares them, `Blob` does not).
//!
//! A shape is a built-in buffer class by name ([`crate::value::DispatchShape::Blob`],
//! [`crate::value::DispatchShape::Buf`]); a user subclass or a class composed over a buffer
//! has no shape and reaches the same handlers through the cascade's arms.

use super::{Handler, MethodRow, RowFlags};
use crate::symbol::Symbol;
use crate::value::value_buf::{
    buf_elem_type_name, buf_elem_width, buf_elems, buf_elems_or_empty, buf_len_or_zero,
    buf_storage, elem_hex, make_buf, set_buf_storage,
};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! rows {
    ($($name:literal => $handler:ident),* $(,)?) => {
        &[$(
            MethodRow {
                owner: "Blob",
                name: $name,
                arity: 0,
                handler: Handler::Narrow($handler),
                flags: RowFlags::NONE,
                named: &[],
            },
            MethodRow {
                owner: "Buf",
                name: $name,
                arity: 0,
                handler: Handler::Narrow($handler),
                flags: RowFlags::NONE,
                named: &[],
            },
        )*]
    };
}

pub(super) static ROWS: &[MethodRow] = rows! {
    "elems" => elems,
    "bytes" => bytes,
    "of" => of,
    "list" => list,
    "contents" => list,
    "reverse" => reverse,
    "Bool" => truthiness,
    "gist" => gist,
    "raku" => raku,
    "Str" => str_of,
};

/// `Blob` declares `Buf` but not `Blob`; `Buf` declares both.
pub(super) static COERCE_ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Blob",
        name: "Buf",
        arity: 0,
        handler: Handler::Narrow(to_buf),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Buf",
        name: "Buf",
        arity: 0,
        handler: Handler::Narrow(to_buf),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Buf",
        name: "Blob",
        arity: 0,
        handler: Handler::Narrow(to_blob),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// The class name of a buffer receiver, when it is a plain buffer instance.
// Cost: O(1).
fn class_of(target: &Value) -> Option<Symbol> {
    match target.view() {
        ValueView::Instance { class_name, .. } => Some(class_name),
        _ => None,
    }
}

/// `.elems`: the number of elements.
// Cost: O(1).
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => {
            Some(Ok(Value::int(buf_len_or_zero(&attributes) as i64)))
        }
        _ => None,
    }
}

/// `.bytes`: the size in bytes, the element count times the element width.
// Cost: O(1).
pub(crate) fn bytes(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => {
            let width = buf_elem_width(&class_name.resolve()) as i64;
            Some(Ok(Value::int(buf_len_or_zero(&attributes) as i64 * width)))
        }
        _ => None,
    }
}

/// `.of`: the element type object (`Buf.new(1).of` is `(uint8)`).
// Cost: O(1).
pub(crate) fn of(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = class_of(target)?;
    Some(Ok(Value::package(Symbol::intern(&buf_elem_type_name(
        &class.resolve(),
    )))))
}

/// `.list` and `.contents`: the elements as a `List` of integers.
// Cost: O(e), e = elements.
pub(crate) fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => {
            Some(Ok(Value::array(buf_elems_or_empty(&attributes))))
        }
        _ => None,
    }
}

/// `.reverse`: a buffer of the same class with the elements reversed.
// Cost: O(e), e = elements.
pub(crate) fn reverse(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => {
            let mut items = buf_elems_or_empty(&attributes);
            items.reverse();
            Some(Ok(make_buf(class_name, items)))
        }
        _ => None,
    }
}

/// `.Bool`: whether the buffer has an element.
// Cost: O(1).
pub(crate) fn truthiness(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    class_of(target)?;
    Some(Ok(Value::truth(target.truthy())))
}

/// `.gist`: the elements in hex, at most 100 (`Buf:0x<01 02>`).
// Cost: O(min(e, 100)), e = elements.
pub(crate) fn gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    render(target, false)
}

/// `.raku`: the constructor form (`Buf[uint8].new(1,2)`).
// Cost: O(e), e = elements.
pub(crate) fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    render(target, true)
}

/// [`gist`] and [`raku`], and the cascade's `.gist`/`.raku`/`.perl` of a
/// buffer: the one rendering.
// Cost: O(e) for `.raku`, O(min(e, 100)) for `.gist`, e = elements.
pub(crate) fn render(target: &Value, raku: bool) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    let Some(items) = buf_elems(&attributes) else {
        return Some(Ok(Value::str(format!("{class_name}()"))));
    };
    let cn = class_name.resolve();
    if raku {
        // `to_string_value`, not an `Int`-only match: a `uint64` element above
        // `i64::MAX` decodes to a `BigInt`.
        let elems: Vec<String> = items.iter().map(Value::to_string_value).collect();
        let canonical = match cn.as_str() {
            "buf8" => "Buf[uint8]",
            "buf16" => "Buf[uint16]",
            "buf32" => "Buf[uint32]",
            "buf64" => "Buf[uint64]",
            "blob8" => "Blob[uint8]",
            "blob16" => "Blob[uint16]",
            "blob32" => "Blob[uint32]",
            "blob64" => "Blob[uint64]",
            other => other,
        };
        return Some(Ok(Value::str(format!(
            "{}.new({})",
            canonical,
            elems.join(",")
        ))));
    }
    // An empty buffer gists as `Blob:0x<>`, not `Blob()`, which is the type
    // object's spelling.
    if items.is_empty() {
        return Some(Ok(Value::str(format!("{class_name}:0x<>"))));
    }
    let width = buf_elem_width(&cn);
    let truncated = items.len() > 100;
    let shown = if truncated { &items[..100] } else { &items[..] };
    let mut hex: Vec<String> = shown.iter().map(|b| elem_hex(b, width)).collect();
    if truncated {
        hex.push("...".to_string());
    }
    Some(Ok(Value::str(format!(
        "{}:0x<{}>",
        class_name,
        hex.join(" ")
    ))))
}

/// `.Str`: `utf8` decodes; every other buffer dies with `X::Buf::AsStr`.
// Cost: O(e) for `utf8`, O(1) otherwise, e = elements.
pub(crate) fn str_of(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    str_or_stringy(target, "Str")
}

/// `.Str` and `.Stringy` of a buffer; `method` names the one called, for the
/// exception.
// Cost: O(e) for `utf8`, O(1) otherwise, e = elements.
pub(crate) fn str_or_stringy(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    let class = class_of(target)?;
    if class.resolve() == "utf8"
        && let Some(decoded) = crate::builtins::decode_buf_method(target, Some("utf-8"))
    {
        return Some(decoded);
    }
    Some(Err(crate::runtime::Interpreter::buf_as_str_error(
        target, method,
    )))
}

/// `.Blob`: a `Blob` over the same bytes.
// Cost: O(1), the storage is shared.
pub(crate) fn to_blob(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    reinterpret(target, "Blob")
}

/// `.Buf`: a `Buf` over the same bytes.
// Cost: O(1), the storage is shared.
pub(crate) fn to_buf(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    reinterpret(target, "Buf")
}

/// `.Buf`/`.Blob` on a byte string: a plain value of the requested class
/// carrying the same storage, so `"\r\n".encode.Buf` is a `Buf[uint8]`.
// Cost: O(1), the storage is shared.
pub(crate) fn reinterpret(target: &Value, class: &str) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let mut attrs = crate::value::AttrMap::new();
    set_buf_storage(
        &mut attrs,
        buf_storage(&attributes.as_map()).unwrap_or_else(|| Value::array(Vec::new())),
    );
    Some(Ok(Value::make_instance(Symbol::intern(class), attrs)))
}
