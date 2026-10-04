//! Where a decoder object keeps the streaming decoder's state
//! ([`crate::builtins::stream_decoder`]), and the one way both of its
//! front ends — the `nqp::decoder*` ops and the `Encoding::Decoder::Builtin`
//! methods — load, run and store it.
//!
//! The state lives in the object's own attributes, so it is shared by every
//! alias the way MoarVM's `Decoder` REPR body is:
//!
//! * `encoding` / `translate-nl` / `line-separators` — the configuration;
//! * `bytes` — a private `Buf[uint8]` holding the undecoded bytes. Its
//!   storage drops consumed bytes from the front in O(1) amortized
//!   (`BufBytes`), so taking lines one at a time from a large buffer does
//!   not move the rest each time;
//! * `chars` / `pending` / `scanned` — the decoded-text queues.

use crate::builtins::stream_decoder::{
    Codec, DEFAULT_LINE_SEPARATORS, DecoderConfig, StreamDecoder, TextQueues,
};
use crate::runtime::RuntimeError;
use crate::symbol::Symbol;
use crate::value::{AttrMap, InstanceAttrs, Value, ValueView};

/// The class a decoder object belongs to.
pub(crate) const DECODER_CLASS: &str = "Encoding::Decoder::Builtin";

const ENCODING: &str = "encoding";
const TRANSLATE_NL: &str = "translate-nl";
const LINE_SEPARATORS: &str = "line-separators";
const BYTES: &str = "bytes";
const CHARS: &str = "chars";
const PENDING: &str = "pending";
const SCANNED: &str = "scanned";

/// Read/write access to a decoder object's attributes: a live instance's
/// shared cell (the nqp ops) or a map being updated (the native methods).
pub(crate) trait DecoderSlots {
    fn slot(&self, key: &str) -> Option<Value>;
    fn set_slot(&mut self, key: &str, value: Value);
    /// The slot's value, moved out (an empty `Str` left in its place), so
    /// a queue held there can be grown without copying it.
    fn take_slot(&mut self, key: &str) -> Option<Value>;
}

impl DecoderSlots for AttrMap {
    fn slot(&self, key: &str) -> Option<Value> {
        self.get(key).cloned()
    }
    fn set_slot(&mut self, key: &str, value: Value) {
        self.insert(key, value);
    }
    fn take_slot(&mut self, key: &str) -> Option<Value> {
        self.insert(key, Value::str(String::new()))
    }
}

/// A live decoder instance's attribute cell.
pub(crate) struct InstanceSlots<'a>(pub &'a InstanceAttrs);

impl DecoderSlots for InstanceSlots<'_> {
    fn slot(&self, key: &str) -> Option<Value> {
        self.0.as_map().get(key).cloned()
    }
    fn set_slot(&mut self, key: &str, value: Value) {
        self.0.write_keys(vec![(Symbol::intern(key), Some(value))]);
    }
    fn take_slot(&mut self, key: &str) -> Option<Value> {
        let old = self.slot(key);
        self.set_slot(key, Value::str(String::new()));
        old
    }
}

/// Whether the object has been configured.
pub(crate) fn is_configured(slots: &impl DecoderSlots) -> bool {
    slots.slot(BYTES).is_some()
}

/// `decoderconfigure` / `Encoding::Decoder::Builtin.new`: set the encoding
/// and start with empty queues.
// Cost: O(1).
pub(crate) fn configure(
    slots: &mut impl DecoderSlots,
    encoding: &str,
    translate_nl: bool,
) -> Result<(), RuntimeError> {
    if is_configured(slots) {
        return Err(RuntimeError::new("Decoder already configured"));
    }
    if Codec::from_label(encoding).is_none() {
        return Err(RuntimeError::new(format!(
            "Unknown string encoding: '{encoding}'"
        )));
    }
    slots.set_slot(ENCODING, Value::str_from(encoding));
    slots.set_slot(TRANSLATE_NL, Value::truth(translate_nl));
    slots.set_slot(
        BYTES,
        crate::value::value_buf::make_buf(Symbol::intern("Buf[uint8]"), Vec::new()),
    );
    store_queues(slots, TextQueues::default());
    Ok(())
}

/// `decoderaddbytes` / `.add-bytes`: append a buffer's bytes (its raw
/// storage, so a `buf16` adds two bytes per element, as MoarVM does).
// Cost: O(k) amortized, k = bytes added.
pub(crate) fn add_bytes(slots: &mut impl DecoderSlots, blob: &Value) -> Result<(), RuntimeError> {
    let added = match blob.view() {
        ValueView::Instance { attributes, .. } => {
            crate::value::value_buf::buf_raw_bytes(&attributes)
        }
        _ => None,
    }
    .ok_or_else(|| RuntimeError::new("Can only add bytes from an int array to a decoder"))?;
    let store = bytes_store(slots)?;
    let ValueView::Instance { attributes, .. } = store.view() else {
        return Err(not_configured());
    };
    crate::value::value_buf::with_buf_storage_mut(&attributes, |bytes, _| {
        bytes.extend_from_slice(&added)
    });
    Ok(())
}

/// `decodersetlineseps` / `.set-line-separators`.
// Cost: O(s), s = total length of the separators.
pub(crate) fn set_line_separators(
    slots: &mut impl DecoderSlots,
    separators: Vec<String>,
) -> Result<(), RuntimeError> {
    bytes_store(slots)?;
    slots.set_slot(
        LINE_SEPARATORS,
        Value::array(separators.into_iter().map(Value::str).collect()),
    );
    // A different separator set invalidates the line-scan position.
    let mut q = load_queues(slots);
    q.scanned = 0;
    store_queues(slots, q);
    Ok(())
}

/// Run one operation on the decoder held in `slots`.
// Cost: O(s) to load the configuration, s = total length of the line
// separators, plus `f`'s own cost.
pub(crate) fn with_decoder<R>(
    slots: &mut impl DecoderSlots,
    f: impl FnOnce(&mut StreamDecoder<'_>) -> Result<R, RuntimeError>,
) -> Result<R, RuntimeError> {
    let store = bytes_store(slots)?;
    let cfg = load_config(slots)?;
    let ValueView::Instance { attributes, .. } = store.view() else {
        return Err(not_configured());
    };
    let mut q = load_queues(slots);
    let out = crate::value::value_buf::with_buf_storage_mut(&attributes, |bytes, _| {
        f(&mut StreamDecoder {
            cfg: &cfg,
            bytes,
            q: &mut q,
        })
    });
    store_queues(slots, q);
    out.ok_or_else(not_configured)?
}

/// A decoder in a map of its own, for `Encoding.decoder` and `.new`.
pub(crate) fn new_decoder(encoding: &str, translate_nl: bool) -> Result<Value, RuntimeError> {
    let mut attrs = AttrMap::new();
    configure(&mut attrs, encoding, translate_nl)?;
    Ok(Value::make_instance(Symbol::intern(DECODER_CLASS), attrs))
}

fn not_configured() -> RuntimeError {
    RuntimeError::new("Decoder not yet configured")
}

fn bytes_store(slots: &impl DecoderSlots) -> Result<Value, RuntimeError> {
    slots.slot(BYTES).ok_or_else(not_configured)
}

fn load_config(slots: &impl DecoderSlots) -> Result<DecoderConfig, RuntimeError> {
    let encoding = slots
        .slot(ENCODING)
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let codec = Codec::from_label(&encoding).ok_or_else(not_configured)?;
    let line_separators = match slots.slot(LINE_SEPARATORS) {
        Some(v) => match v.view() {
            ValueView::Array(items, ..) => items.iter().map(|s| s.to_string_value()).collect(),
            _ => vec![v.to_string_value()],
        },
        None => DEFAULT_LINE_SEPARATORS
            .iter()
            .map(|s| s.to_string())
            .collect(),
    };
    Ok(DecoderConfig {
        codec,
        translate_nl: slots.slot(TRANSLATE_NL).is_some_and(|v| v.truthy()),
        line_separators,
    })
}

fn load_queues(slots: &mut impl DecoderSlots) -> TextQueues {
    let scanned = slots
        .slot(SCANNED)
        .and_then(|v| v.as_int())
        .and_then(|n| usize::try_from(n).ok())
        .unwrap_or(0);
    let mut text = |key| {
        slots
            .take_slot(key)
            .map(Value::into_owned_string)
            .unwrap_or_default()
    };
    TextQueues {
        chars: text(CHARS),
        pending: text(PENDING),
        scanned,
    }
}

fn store_queues(slots: &mut impl DecoderSlots, q: TextQueues) {
    slots.set_slot(CHARS, Value::str(q.chars));
    slots.set_slot(PENDING, Value::str(q.pending));
    slots.set_slot(SCANNED, Value::int(q.scanned as i64));
}
