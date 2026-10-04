//! The per-encoding half of the streaming decoder: turning the bytes at the
//! front of the undecoded queue into text, one unit (one code point, or one
//! utf8-c8 synthetic) at a time.
//!
//! Decoding a unit at a time is what lets the state machine in the parent
//! module stop exactly where MoarVM's decoder stops — after the line
//! separator, or after the requested number of chars — so the bytes behind
//! that point stay raw and `decodertakebytes` / `consume-exactly-bytes` can
//! still hand them out (an HTTP parser reads the header lines and then the
//! body bytes from one decoder).

use crate::value::RuntimeError;

/// A decoder's encoding, resolved once at configuration time.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Codec {
    Utf8,
    Utf8C8,
    Utf16Le,
    Utf16Be,
    Ascii,
    Latin1,
    /// Any other `encoding_rs` encoding (windows-125x, shift_jis, ...).
    Rs(&'static encoding_rs::Encoding),
}

/// Which trailing text a codec keeps back from the decoded-chars queue until
/// more input (or the end of the stream) settles it.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Hold {
    /// The last grapheme, unless it ends in a hard break (MoarVM's NFG
    /// normalizer: a following combining mark could still join it).
    Grapheme,
    /// Only a trailing `\r`, which a `\n` could turn into the `\r\n`
    /// grapheme (MoarVM's single-byte decoders).
    CarriageReturn,
    /// Nothing (MoarVM's UTF-16 decoder).
    Nothing,
}

impl Codec {
    /// The codec for an encoding name, through the alias table `.decode`
    /// uses. `None` for an unknown name.
    pub(crate) fn from_label(label: &str) -> Option<Codec> {
        Some(
            match crate::builtins::normalize_builtin_encoding_label(label)?.as_str() {
                "utf-8" => Codec::Utf8,
                "utf8-c8" => Codec::Utf8C8,
                // MoarVM's streaming `utf16` decoder reads native (little-endian)
                // code units and does not sniff a BOM.
                "utf-16" | "utf-16le" => Codec::Utf16Le,
                "utf-16be" => Codec::Utf16Be,
                "ascii" => Codec::Ascii,
                "iso-8859-1" => Codec::Latin1,
                "windows-932" => Codec::Rs(encoding_rs::SHIFT_JIS),
                other => Codec::Rs(encoding_rs::Encoding::for_label(other.as_bytes())?),
            },
        )
    }

    pub(crate) fn hold(self) -> Hold {
        match self {
            Codec::Utf8 | Codec::Utf8C8 => Hold::Grapheme,
            Codec::Utf16Le | Codec::Utf16Be => Hold::Nothing,
            Codec::Ascii | Codec::Latin1 | Codec::Rs(_) => Hold::CarriageReturn,
        }
    }

    /// Whether released text is NFC-normalized. utf8-c8 exists to round-trip
    /// bytes exactly, so it is the one codec that is not.
    pub(crate) fn normalizes(self) -> bool {
        self != Codec::Utf8C8
    }

    /// Decode the unit at the front of `bytes` onto `out`, answering how many
    /// bytes it took. `Ok(None)` when `bytes` is empty or holds only the
    /// start of a unit.
    // Cost: O(1) (a unit is at most four bytes).
    pub(crate) fn decode_unit(
        self,
        bytes: &[u8],
        out: &mut String,
    ) -> Result<Option<usize>, RuntimeError> {
        let Some(&b0) = bytes.first() else {
            return Ok(None);
        };
        match self {
            Codec::Utf8 => match utf8_unit(bytes)? {
                Utf8Unit::Char(c, n) => {
                    out.push(c);
                    Ok(Some(n))
                }
                Utf8Unit::Incomplete => Ok(None),
            },
            Codec::Utf8C8 => match utf8_unit(bytes) {
                Ok(Utf8Unit::Char(c, n)) => {
                    out.push(c);
                    Ok(Some(n))
                }
                Ok(Utf8Unit::Incomplete) => Ok(None),
                // An invalid byte becomes its synthetic, and decoding goes on
                // after it.
                Err(_) => {
                    out.push_str(&crate::runtime::utf8_c8::decode_utf8_c8(&bytes[..1]));
                    Ok(Some(1))
                }
            },
            Codec::Utf16Le | Codec::Utf16Be => utf16_unit(bytes, self == Codec::Utf16Be, out),
            Codec::Ascii => {
                if b0 > 0x7F {
                    return Err(RuntimeError::new(format!(
                        "Will not decode invalid ASCII (code point ({b0}) > 127 found)"
                    )));
                }
                out.push(b0 as char);
                Ok(Some(1))
            }
            Codec::Latin1 => {
                out.push(b0 as char);
                Ok(Some(1))
            }
            Codec::Rs(enc) => rs_unit(enc, bytes, out),
        }
    }

    /// Decode as many whole units from the front of `bytes` as there are,
    /// onto `out`. Answers the bytes taken for what was decoded, and the
    /// error that stopped the run early, if one did. Equivalent to calling
    /// [`Self::decode_unit`] until it answers `None`, but UTF-8 validates the
    /// whole run at once.
    // Cost: O(b), b = bytes decoded.
    pub(crate) fn decode_run(
        self,
        bytes: &[u8],
        out: &mut String,
    ) -> (usize, Option<RuntimeError>) {
        if self == Codec::Utf8 {
            let end = complete_utf8_prefix(bytes);
            return match std::str::from_utf8(&bytes[..end]) {
                Ok(s) => {
                    out.push_str(s);
                    (end, None)
                }
                Err(e) => {
                    let good = e.valid_up_to();
                    out.push_str(std::str::from_utf8(&bytes[..good]).unwrap_or_default());
                    // Re-decode the offending unit for MoarVM's message.
                    let err = utf8_unit(&bytes[good..])
                        .err()
                        .unwrap_or_else(|| malformed_utf8(bytes[good]));
                    (good, Some(err))
                }
            };
        }
        let mut taken = 0;
        loop {
            match self.decode_unit(&bytes[taken..], out) {
                Ok(Some(n)) => taken += n,
                Ok(None) => return (taken, None),
                Err(e) => return (taken, Some(e)),
            }
        }
    }
}

/// MoarVM's error for bytes left over at the end of a stream.
pub(crate) fn incomplete_error(bytes: &[u8]) -> RuntimeError {
    let hex: Vec<String> = bytes.iter().map(|b| format!("{b:02x}")).collect();
    RuntimeError::new(format!(
        "Incomplete character near bytes {} at the end of a stream",
        hex.join(" ")
    ))
}

fn malformed_utf8(byte: u8) -> RuntimeError {
    RuntimeError::new(format!("Malformed UTF-8 near byte {byte:02x}"))
}

enum Utf8Unit {
    Char(char, usize),
    Incomplete,
}

/// The UTF-8 sequence at the front of `bytes` (non-empty).
fn utf8_unit(bytes: &[u8]) -> Result<Utf8Unit, RuntimeError> {
    let b0 = bytes[0];
    let need = match b0 {
        0x00..=0x7F => return Ok(Utf8Unit::Char(b0 as char, 1)),
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
        _ => return Err(malformed_utf8(b0)),
    };
    let have = bytes.len().min(need);
    if let Some(&bad) = bytes[1..have].iter().find(|b| *b & 0xC0 != 0x80) {
        return Err(malformed_utf8(bad));
    }
    if have < need {
        return Ok(Utf8Unit::Incomplete);
    }
    match std::str::from_utf8(&bytes[..need]) {
        Ok(s) => Ok(Utf8Unit::Char(s.chars().next().unwrap_or_default(), need)),
        // An overlong form or a surrogate: the second byte is out of range.
        Err(_) => Err(malformed_utf8(bytes[1])),
    }
}

/// The length of the longest prefix of `bytes` that does not end inside a
/// UTF-8 sequence (whether or not that prefix is valid).
fn complete_utf8_prefix(bytes: &[u8]) -> usize {
    let len = bytes.len();
    // A sequence is at most four bytes, so only the last three can be the
    // start of an unfinished one.
    for back in 1..=len.min(3) {
        let b = bytes[len - back];
        if b & 0xC0 == 0x80 {
            continue; // a continuation byte: keep looking for its lead
        }
        let need = match b {
            0xC2..=0xDF => 2,
            0xE0..=0xEF => 3,
            0xF0..=0xF4 => 4,
            _ => return len, // ASCII or an invalid lead: nothing pending
        };
        return if back < need { len - back } else { len };
    }
    len
}

fn utf16_unit(
    bytes: &[u8],
    big_endian: bool,
    out: &mut String,
) -> Result<Option<usize>, RuntimeError> {
    let unit = |i: usize| -> Option<u16> {
        let pair = [*bytes.get(i)?, *bytes.get(i + 1)?];
        Some(if big_endian {
            u16::from_be_bytes(pair)
        } else {
            u16::from_le_bytes(pair)
        })
    };
    let Some(first) = unit(0) else {
        return Ok(None);
    };
    let unpaired = || RuntimeError::new("Malformed UTF-16 string: unpaired surrogate (line 1)");
    match first {
        0xD800..=0xDBFF => {
            let Some(second) = unit(2) else {
                return Ok(None);
            };
            match char::decode_utf16([first, second]).next() {
                Some(Ok(c)) => {
                    out.push(c);
                    Ok(Some(4))
                }
                _ => Err(unpaired()),
            }
        }
        0xDC00..=0xDFFF => Err(unpaired()),
        _ => {
            out.push(char::from_u32(first as u32).unwrap_or_default());
            Ok(Some(2))
        }
    }
}

/// One character of an `encoding_rs` encoding: the shortest prefix (of at
/// most four bytes) that decodes cleanly.
fn rs_unit(
    enc: &'static encoding_rs::Encoding,
    bytes: &[u8],
    out: &mut String,
) -> Result<Option<usize>, RuntimeError> {
    let longest = if enc.is_single_byte() { 1 } else { 4 };
    for len in 1..=bytes.len().min(longest) {
        if let Some(text) = enc.decode_without_bom_handling_and_without_replacement(&bytes[..len])
            && !text.is_empty()
        {
            out.push_str(&text);
            return Ok(Some(len));
        }
    }
    if bytes.len() < longest {
        return Ok(None);
    }
    Err(RuntimeError::new(format!(
        "Error decoding {} near byte {:02x}",
        enc.name(),
        bytes[0]
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn complete_prefix_stops_before_an_unfinished_sequence() {
        assert_eq!(complete_utf8_prefix(b"abc"), 3);
        assert_eq!(complete_utf8_prefix(&[0x61, 0xC3]), 1);
        assert_eq!(complete_utf8_prefix(&[0x61, 0xC3, 0xA9]), 3);
        assert_eq!(complete_utf8_prefix(&[0xE2, 0x82]), 0);
        assert_eq!(complete_utf8_prefix(&[0xF0, 0x9F, 0x98]), 0);
        assert_eq!(complete_utf8_prefix(&[0xF0, 0x9F, 0x98, 0x80]), 4);
    }

    #[test]
    fn utf8_units_report_moarvm_errors() {
        let mut out = String::new();
        assert!(matches!(
            Codec::Utf8.decode_unit(&[0xC3], &mut out),
            Ok(None)
        ));
        let err = Codec::Utf8
            .decode_unit(&[0xC3, 0x28], &mut out)
            .unwrap_err();
        assert_eq!(err.message, "Malformed UTF-8 near byte 28");
        let err = Codec::Utf8.decode_unit(&[0xFF], &mut out).unwrap_err();
        assert_eq!(err.message, "Malformed UTF-8 near byte ff");
    }

    #[test]
    fn utf8_c8_turns_a_bad_byte_into_its_synthetic() {
        let mut out = String::new();
        assert_eq!(Codec::Utf8C8.decode_run(&[0x61, 0xFF, 0x62], &mut out).0, 3);
        assert_eq!(
            crate::runtime::utf8_c8::encode_utf8_c8(&out),
            vec![0x61, 0xFF, 0x62]
        );
    }

    #[test]
    fn utf16_waits_for_the_second_half_of_a_pair() {
        let mut out = String::new();
        assert!(matches!(
            Codec::Utf16Le.decode_unit(&[0x3D, 0xD8], &mut out),
            Ok(None)
        ));
        assert_eq!(
            Codec::Utf16Le
                .decode_unit(&[0x3D, 0xD8, 0x00, 0xDE], &mut out)
                .unwrap(),
            Some(4)
        );
        assert_eq!(out, "\u{1F600}");
    }
}
