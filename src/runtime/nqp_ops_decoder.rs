//! The stream-decoding `nqp::` ops (#11503): `decoderconfigure`,
//! `decoderaddbytes`, `decodertakeline`, `decodertakechars`, ...
//!
//! Each runs the one streaming decoder ([`crate::builtins::stream_decoder`])
//! over the state a decoder object keeps in its attributes
//! ([`super::stream_decoder_object`]) — the same state, through the same
//! routines, as the `Encoding::Decoder::Builtin` methods Rakudo builds on
//! these ops.

use super::stream_decoder_object::{self as decoder, InstanceSlots};
use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// How many operands each op takes (MoarVM's ops have a fixed count, and
/// Rakudo rejects another at compile time).
// Cost: O(1).
fn operand_count(op: &str) -> Option<usize> {
    Some(match op {
        "decodertakeallchars"
        | "decodertakeavailablechars"
        | "decoderbytesavailable"
        | "decoderempty" => 1,
        "decodersetlineseps" | "decoderaddbytes" | "decodertakechars" | "decodertakecharseof" => 2,
        "decoderconfigure" | "decodertakeline" | "decodertakebytes" => 3,
        _ => return None,
    })
}

fn operand(args: &[Value], i: usize) -> Value {
    args.get(i).cloned().unwrap_or(Value::NIL)
}

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

/// nqp's null string: mutsu's one absent value, which `isnull_s` reads as
/// null.
fn str_or_null(s: Option<String>) -> Value {
    s.map_or(Value::NIL, Value::str)
}

impl Interpreter {
    /// Try a stream-decoding `nqp::` op. `None` means "not an op this table
    /// knows" -- the end of the dispatch chain.
    pub(crate) fn call_nqp_op_decoder(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let want = operand_count(op)?;
        if args.len() != want {
            return Some(Err(RuntimeError::new(format!(
                "Arg count {} doesn't equal required operand count {want} for op '{op}'",
                args.len()
            ))));
        }
        let target = operand(args, 0);
        let attributes = match self.decoder_attributes(op, &target) {
            Ok(attributes) => attributes,
            Err(e) => return Some(Err(e)),
        };
        let mut slots = InstanceSlots(&attributes);
        Some(match op {
            // nqp::decoderconfigure($dec, $encoding, %config): set the
            // encoding; `%config<translate_newlines>` hands `\r\n` out as
            // `\n`. (MoarVM's streaming decoders ignore `replacement`.)
            // Cost: O(1).
            "decoderconfigure" => {
                let translate_nl = match operand(args, 2).view() {
                    ValueView::Hash(map) => map
                        .get("translate_newlines")
                        .is_some_and(|v| crate::runtime::to_int(v) != 0),
                    _ => false,
                };
                decoder::configure(
                    &mut slots,
                    &operand(args, 1).to_string_value(),
                    translate_nl,
                )
                .map(|()| target.clone())
            }
            // nqp::decodersetlineseps($dec, @seps): the line separators
            // `decodertakeline` splits on.
            // Cost: O(s), s = total length of the separators.
            "decodersetlineseps" => {
                let seps = match operand(args, 1).view() {
                    ValueView::Array(items, ..) => {
                        items.iter().map(|s| s.to_string_value()).collect()
                    }
                    _ => Vec::new(),
                };
                decoder::set_line_separators(&mut slots, seps).map(|()| target.clone())
            }
            // nqp::decoderaddbytes($dec, $blob): queue a buffer's bytes;
            // answers the buffer.
            // Cost: O(k) amortized, k = bytes added.
            "decoderaddbytes" => {
                let blob = operand(args, 1);
                decoder::add_bytes(&mut slots, &blob).map(|()| blob)
            }
            // nqp::decodertakechars($dec, $n): exactly $n chars, or null;
            // `decodertakecharseof` hands out fewer at the end of the stream.
            // Cost: O(k + d), k = chars taken, d = bytes decoded to reach them.
            "decodertakechars" | "decodertakecharseof" => {
                let n = usize::try_from(iarg(args, 1)).unwrap_or(0);
                let eof = op == "decodertakecharseof";
                decoder::with_decoder(&mut slots, |d| d.take_chars(n, eof)).map(str_or_null)
            }
            // nqp::decodertakeavailablechars($dec): every char that is final.
            // Cost: O(b + c), b = undecoded bytes, c = chars taken.
            "decodertakeavailablechars" => {
                decoder::with_decoder(&mut slots, |d| d.take_available_chars()).map(Value::str)
            }
            // nqp::decodertakeallchars($dec): everything (the end of the
            // stream); an incomplete trailing character is an error.
            // Cost: O(b + c), b = undecoded bytes, c = chars taken.
            "decodertakeallchars" => {
                decoder::with_decoder(&mut slots, |d| d.take_all_chars()).map(Value::str)
            }
            // nqp::decodertakeline($dec, $chomp, $incomplete-ok): the next
            // line, or null; with $incomplete-ok the rest of the stream is
            // the last line.
            // Cost: O(l + d), l = chars of the line, d = bytes decoded to find it.
            "decodertakeline" => {
                let chomp = iarg(args, 1) != 0;
                let eof = iarg(args, 2) != 0;
                decoder::with_decoder(&mut slots, |d| d.take_line(chomp, eof)).map(str_or_null)
            }
            // nqp::decoderbytesavailable($dec): undecoded bytes buffered.
            // Cost: O(1).
            "decoderbytesavailable" => {
                decoder::with_decoder(&mut slots, |d| Ok(d.bytes.len() as i64)).map(Value::int)
            }
            // nqp::decoderempty($dec): 1 when nothing is buffered at all.
            // Cost: O(1).
            "decoderempty" => {
                decoder::with_decoder(&mut slots, |d| Ok(i64::from(d.is_empty()))).map(Value::int)
            }
            // nqp::decodertakebytes($dec, $buf-type, $n): the next $n
            // undecoded bytes as a buffer of $buf-type's class, or null.
            // Cost: O(n).
            "decodertakebytes" => {
                let class = match operand(args, 1).view() {
                    ValueView::Instance { class_name, .. } => class_name,
                    ValueView::Package(name) => name,
                    _ => Symbol::intern("Buf[uint8]"),
                };
                let n = usize::try_from(iarg(args, 2)).unwrap_or(0);
                decoder::with_decoder(&mut slots, |d| Ok(d.take_bytes(n))).map(|taken| {
                    taken.map_or(Value::NIL, |bytes| {
                        crate::value::value_buf::make_buf_from_bytes(class, &bytes)
                    })
                })
            }
            _ => return None,
        })
    }

    /// The attribute cell of an object with the Decoder representation: an
    /// instance of `Encoding::Decoder::Builtin` (or a subclass).
    fn decoder_attributes<'v>(
        &mut self,
        op: &str,
        target: &'v Value,
    ) -> Result<crate::value::GcRef<'v, crate::value::InstanceAttrs>, RuntimeError> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && self.is_stream_decoder_class(&class_name.resolve())
        {
            return Ok(attributes);
        }
        Err(RuntimeError::new(format!(
            "Operation '{op}' can only work on an object with the Decoder representation"
        )))
    }
}
