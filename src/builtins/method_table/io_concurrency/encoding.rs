//! The rows of the built-in encodings (ADR-11276 §9.156): `Encoding::Builtin`
//! (what `Encoding::Registry.find` answers), its `Encoding::Encoder::Builtin`
//! and its streaming `Encoding::Decoder::Builtin`.
//!
//! Each has an opaque dispatch shape (`DispatchShape::is_opaque`), so every
//! call entry reaches the rows through the table; a user subclass of the
//! decoder reaches them through its owner (`invoke_owner`). A decoder keeps its state in its
//! own attribute cell (`stream_decoder_object`), which its rows edit in place,
//! the same routines the `nqp::decoder*` ops run.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::stream_decoder_object::{self as decoder, InstanceSlots};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:expr) => {
        row!($owner, $name, $arity, $handler, &[])
    };
    ($owner:literal, $name:literal, $arity:literal, $handler:expr, $named:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            // A decoder's rows edit its queues in its shared cell.
            flags: RowFlags::ANY_ARGS
                .or(RowFlags::OBJECT_ARGS)
                .or(RowFlags::EFFECTS),
            named: $named,
        }
    };
}

const DECODER: &str = "Encoding::Decoder::Builtin";

pub(super) static ROWS: &[MethodRow] = &[
    row!("Encoding::Builtin", "name", 0, Handler::Narrow(name)),
    row!(
        "Encoding::Builtin",
        "alternative-names",
        0,
        Handler::Narrow(alternative_names)
    ),
    row!(
        "Encoding::Builtin",
        "encoder",
        0,
        Handler::Interp(encoder),
        &["replacement", "translate-nl"]
    ),
    row!(
        "Encoding::Builtin",
        "decoder",
        0,
        Handler::Interp(decoder),
        &["translate-nl"]
    ),
    row!(
        "Encoding::Encoder::Builtin",
        "encode-chars",
        1,
        Handler::Interp(encode_chars)
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "add-bytes",
        1,
        Handler::Narrow(add_bytes)
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "consume-available-chars",
        0,
        Handler::Narrow(consume_available_chars)
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "consume-all-chars",
        0,
        Handler::Narrow(consume_all_chars)
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "consume-exactly-chars",
        1,
        Handler::Interp(consume_exactly_chars),
        &["eof"]
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "consume-line-chars",
        0,
        Handler::Interp(consume_line_chars),
        &["chomp", "eof"]
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "consume-exactly-bytes",
        1,
        Handler::Narrow(consume_exactly_bytes)
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "set-line-separators",
        1,
        Handler::Narrow(set_line_separators)
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "bytes-available",
        0,
        Handler::Narrow(bytes_available)
    ),
    row!(
        "Encoding::Decoder::Builtin",
        "is-empty",
        0,
        Handler::Narrow(is_empty)
    ),
];

/// The decoder owner, for a user subclass of `Encoding::Decoder::Builtin`.
pub(crate) const DECODER_OWNERS: &[&str] = &[DECODER];

/// The methods `owner` declares here: the native methods `runtime_init`
/// registers for the class, derived so they cannot drift from the rows.
// Cost: O(r), r = rows of the family.
pub(crate) fn names(owner: &str) -> impl Iterator<Item = &'static str> + '_ {
    ROWS.iter()
        .filter(move |row| row.owner == owner)
        .map(|row| row.name)
}

/// The attribute `name` of an instance as a string, empty when it has none.
// Cost: O(n), n = chars of the attribute.
fn text(target: &Value, name: &str) -> String {
    match target.view() {
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get(name)
            .map(Value::to_string_value)
            .unwrap_or_default(),
        _ => String::new(),
    }
}

/// `Encoding::Builtin.name`.
// Cost: O(n), n = chars of the name.
fn name(target: &Value, _: &[Value]) -> Answer {
    Some(Ok(Value::str(text(target, "name"))))
}

/// `Encoding::Builtin.alternative-names`.
// Cost: O(1).
fn alternative_names(target: &Value, _: &[Value]) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(Ok(attributes
        .as_map()
        .get("alternative-names")
        .cloned()
        .unwrap_or_else(|| Value::array(Vec::new()))))
}

/// `Encoding::Builtin.encoder(:$replacement)`: an encoder for this encoding.
// Cost: O(1).
fn encoder(_: &mut Interpreter, target: &Value, _: &[Value], named: Named<'_>) -> Answer {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("encoding".to_string(), Value::str(text(target, "name")));
    if let Some(replacement) = named.get("replacement") {
        attrs.insert("replacement".to_string(), replacement.clone());
    }
    Some(Ok(Value::make_instance(
        Symbol::intern("Encoding::Encoder::Builtin"),
        attrs,
    )))
}

/// `Encoding::Builtin.decoder(:$translate-nl)`: a streaming decoder for this
/// encoding.
// Cost: O(1).
fn decoder(_: &mut Interpreter, target: &Value, _: &[Value], named: Named<'_>) -> Answer {
    let translate_nl = named.get("translate-nl").is_some_and(Value::truthy);
    Some(decoder::new_decoder(&text(target, "name"), translate_nl))
}

/// `Encoding::Encoder::Builtin.encode-chars($str)`.
// Cost: O(n + p + r), n = input bytes, p = replacement bytes, r = output bytes.
fn encode_chars(interp: &mut Interpreter, target: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let map = attributes.as_map();
    Some(interp.encode_chars(
        &text(target, "encoding"),
        map.get("replacement"),
        &args[0].to_string_value(),
    ))
}

/// Run `f` on the decoder state the receiver's own cell holds.
// Cost: O(1) plus `f`'s.
fn with_slots<R>(
    target: &Value,
    f: impl FnOnce(&mut InstanceSlots<'_>) -> Result<R, RuntimeError>,
) -> Option<Result<R, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(f(&mut InstanceSlots(&attributes)))
}

/// A taken string, or the `Str` type object where the op gives null.
// Cost: O(1).
fn str_or_type(s: Option<String>) -> Value {
    s.map_or_else(|| Value::package(Symbol::intern("Str")), Value::str)
}

/// A count argument as a `usize`; a negative or non-numeric one is 0.
// Cost: O(1).
fn count(arg: &Value) -> usize {
    usize::try_from(crate::runtime::to_int(arg)).unwrap_or(0)
}

// Cost: O(k) amortized, k = bytes added.
fn add_bytes(target: &Value, args: &[Value]) -> Answer {
    with_slots(target, |slots| {
        decoder::add_bytes(slots, &args[0]).map(|()| Value::NIL)
    })
}

// Cost: O(b + c), b = undecoded bytes, c = chars taken.
fn consume_available_chars(target: &Value, _: &[Value]) -> Answer {
    with_slots(target, |slots| {
        decoder::with_decoder(slots, |d| d.take_available_chars()).map(Value::str)
    })
}

// Cost: O(b + c), b = undecoded bytes, c = chars taken.
fn consume_all_chars(target: &Value, _: &[Value]) -> Answer {
    with_slots(target, |slots| {
        decoder::with_decoder(slots, |d| d.take_all_chars()).map(Value::str)
    })
}

// Cost: O(k + d), k = chars taken, d = bytes decoded to reach them.
fn consume_exactly_chars(
    _: &mut Interpreter,
    t: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    let (n, eof) = (count(&args[0]), named.get("eof").is_some_and(Value::truthy));
    with_slots(t, |slots| {
        decoder::with_decoder(slots, |d| d.take_chars(n, eof)).map(str_or_type)
    })
}

// Cost: O(l + d), l = chars of the line, d = bytes decoded to find it.
fn consume_line_chars(_: &mut Interpreter, t: &Value, _: &[Value], named: Named<'_>) -> Answer {
    let chomp = named.get("chomp").is_some_and(Value::truthy);
    let eof = named.get("eof").is_some_and(Value::truthy);
    with_slots(t, |slots| {
        decoder::with_decoder(slots, |d| d.take_line(chomp, eof)).map(str_or_type)
    })
}

// Cost: O(n).
fn consume_exactly_bytes(target: &Value, args: &[Value]) -> Answer {
    let n = count(&args[0]);
    with_slots(target, |slots| {
        decoder::with_decoder(slots, |d| Ok(d.take_bytes(n))).map(|taken| match taken {
            Some(bytes) => {
                crate::value::value_buf::make_buf_from_bytes(Symbol::intern("Buf[uint8]"), &bytes)
            }
            None => Value::package(Symbol::intern("Blob")),
        })
    })
}

// Cost: O(s), s = total length of the separators.
fn set_line_separators(target: &Value, args: &[Value]) -> Answer {
    let seps = match args[0].view() {
        ValueView::Array(items, ..) => items.iter().map(|s| s.to_string_value()).collect(),
        _ => vec![args[0].to_string_value()],
    };
    with_slots(target, |slots| {
        decoder::set_line_separators(slots, seps).map(|()| Value::NIL)
    })
}

// Cost: O(1).
fn bytes_available(target: &Value, _: &[Value]) -> Answer {
    with_slots(target, |slots| {
        decoder::with_decoder(slots, |d| Ok(d.bytes.len() as i64)).map(Value::int)
    })
}

// Cost: O(1).
fn is_empty(target: &Value, _: &[Value]) -> Answer {
    with_slots(target, |slots| {
        decoder::with_decoder(slots, |d| Ok(d.is_empty())).map(Value::truth)
    })
}
