//! `Encoding::Decoder::Builtin`'s methods (#11503) — the Raku face of the
//! streaming decoder, as Rakudo writes each of them over an
//! `nqp::decoder*` op. They run the same routines over the same object
//! state as those ops ([`crate::runtime::stream_decoder_object`]).

use crate::runtime::stream_decoder_object::{self as decoder, DECODER_CLASS, InstanceSlots};
use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{AttrMap, InstanceAttrs, Value, ValueView};

/// The value of the named argument `name`, if one was passed.
fn named<'a>(args: &'a [Value], name: &str) -> Option<&'a Value> {
    args.iter().find_map(|arg| match arg.view() {
        ValueView::Pair(key, value) if key == name => Some(value),
        _ => None,
    })
}

fn positional(args: &[Value], i: usize) -> Option<&Value> {
    args.iter()
        .filter(|arg| !matches!(arg.view(), ValueView::Pair(..)))
        .nth(i)
}

fn flag(args: &[Value], name: &str) -> bool {
    named(args, name).is_some_and(Value::truthy)
}

fn count(args: &[Value]) -> usize {
    positional(args, 0)
        .map(crate::runtime::to_int)
        .and_then(|n| usize::try_from(n).ok())
        .unwrap_or(0)
}

/// A taken string, or the `Str` type object where the op gives null.
fn str_or_type(s: Option<String>) -> Value {
    s.map_or_else(|| Value::package(Symbol::intern("Str")), Value::str)
}

impl Interpreter {
    /// Whether `class_name` is (or inherits from) the built-in decoder.
    pub(in crate::runtime) fn is_stream_decoder_class(&mut self, class_name: &str) -> bool {
        class_name == DECODER_CLASS
            || self
                .class_mro(class_name)
                .iter()
                .any(|c| *c == DECODER_CLASS)
    }

    /// A decoder method on a live decoder object; `None` when `method` is
    /// not one of them (or the receiver is not a decoder).
    pub(in crate::runtime) fn try_stream_decoder_method(
        &mut self,
        attributes: &InstanceAttrs,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !is_decoder_method(method) || !self.is_stream_decoder_class(class_name) {
            return None;
        }
        Some(stream_decoder_method(
            &mut InstanceSlots(attributes),
            method,
            args,
        ))
    }

    /// The decoder methods reached with only a snapshot of the attributes
    /// (a qualified call): the queries work on it, the consuming methods
    /// need the object itself.
    pub(in crate::runtime) fn native_stream_decoder(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match method {
            "bytes-available" | "is-empty" => {
                stream_decoder_method(&mut attributes.clone(), method, args)
            }
            "WHAT" => Ok(Value::package(Symbol::intern(DECODER_CLASS))),
            _ => Err(RuntimeError::new(format!(
                "No such method '{method}' for invocant of type '{DECODER_CLASS}'"
            ))),
        }
    }
}

fn is_decoder_method(method: &str) -> bool {
    matches!(
        method,
        "add-bytes"
            | "consume-available-chars"
            | "consume-all-chars"
            | "consume-exactly-chars"
            | "consume-line-chars"
            | "consume-exactly-bytes"
            | "set-line-separators"
            | "bytes-available"
            | "is-empty"
    )
}

fn stream_decoder_method(
    slots: &mut impl decoder::DecoderSlots,
    method: &str,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    match method {
        // Cost: O(k) amortized, k = bytes added.
        "add-bytes" => {
            let blob = positional(args, 0).cloned().unwrap_or(Value::NIL);
            decoder::add_bytes(slots, &blob).map(|()| Value::NIL)
        }
        // Cost: O(b + c), b = undecoded bytes, c = chars taken.
        "consume-available-chars" => {
            decoder::with_decoder(slots, |d| d.take_available_chars()).map(Value::str)
        }
        // Cost: O(b + c), b = undecoded bytes, c = chars taken.
        "consume-all-chars" => decoder::with_decoder(slots, |d| d.take_all_chars()).map(Value::str),
        // Cost: O(k + d), k = chars taken, d = bytes decoded to reach them.
        "consume-exactly-chars" => {
            let (n, eof) = (count(args), flag(args, "eof"));
            decoder::with_decoder(slots, |d| d.take_chars(n, eof)).map(str_or_type)
        }
        // Cost: O(l + d), l = chars of the line, d = bytes decoded to find it.
        "consume-line-chars" => {
            let (chomp, eof) = (flag(args, "chomp"), flag(args, "eof"));
            decoder::with_decoder(slots, |d| d.take_line(chomp, eof)).map(str_or_type)
        }
        // Cost: O(n).
        "consume-exactly-bytes" => {
            let n = count(args);
            decoder::with_decoder(slots, |d| Ok(d.take_bytes(n))).map(|taken| match taken {
                Some(bytes) => crate::value::value_buf::make_buf_from_bytes(
                    Symbol::intern("Buf[uint8]"),
                    &bytes,
                ),
                None => Value::package(Symbol::intern("Blob")),
            })
        }
        // Cost: O(s), s = total length of the separators.
        "set-line-separators" => {
            let seps = match positional(args, 0).map(Value::view) {
                Some(ValueView::Array(items, ..)) => {
                    items.iter().map(|s| s.to_string_value()).collect()
                }
                Some(_) => vec![
                    positional(args, 0)
                        .map(Value::to_string_value)
                        .unwrap_or_default(),
                ],
                None => Vec::new(),
            };
            decoder::set_line_separators(slots, seps).map(|()| Value::NIL)
        }
        // Cost: O(1).
        "bytes-available" => {
            decoder::with_decoder(slots, |d| Ok(d.bytes.len() as i64)).map(Value::int)
        }
        // Cost: O(1).
        "is-empty" => decoder::with_decoder(slots, |d| Ok(d.is_empty())).map(Value::truth),
        _ => Err(RuntimeError::new(format!(
            "No such method '{method}' for invocant of type '{DECODER_CLASS}'"
        ))),
    }
}
