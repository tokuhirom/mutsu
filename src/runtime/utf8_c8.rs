//! UTF-8 Clean-8 (utf8-c8) encoding support.
//!
//! utf8-c8 is Raku's encoding that allows round-tripping arbitrary byte
//! sequences through strings. Valid UTF-8 is decoded normally; invalid
//! bytes are represented as the synthetic codepoint U+10FFFD followed by
//! `xNN`, where `NN` is the original byte in uppercase hexadecimal. Raku's
//! grapheme handling keeps that four-codepoint sequence as one character.
//!
//! When encoding back to utf8-c8, the synthetic marker plus its `xNN` payload
//! is emitted as the corresponding single byte, and all other characters are
//! emitted as standard UTF-8.

/// Marker codepoint for Raku's invalid-byte grapheme representation.
pub(crate) const SYNTHETIC_MARKER: char = '\u{10FFFD}';
pub(crate) const SYNTHETIC_MARKER_STR: &str = "\u{10FFFD}";

fn append_invalid_byte(result: &mut String, byte: u8) {
    result.push(SYNTHETIC_MARKER);
    result.push('x');
    result.push(
        char::from_digit((byte >> 4) as u32, 16)
            .unwrap()
            .to_ascii_uppercase(),
    );
    result.push(
        char::from_digit((byte & 0x0F) as u32, 16)
            .unwrap()
            .to_ascii_uppercase(),
    );
}

/// Decode a byte slice using the utf8-c8 scheme.
///
/// Valid UTF-8 sequences are decoded normally. Each byte that is part of
/// an invalid sequence is represented by the synthetic marker and its `xNN`
/// byte payload.
pub fn decode_utf8_c8(bytes: &[u8]) -> String {
    let mut result = String::new();
    let mut i = 0;
    while i < bytes.len() {
        // Try to decode the longest valid UTF-8 sequence starting at i.
        match std::str::from_utf8(&bytes[i..]) {
            Ok(s) => {
                // Remaining bytes are all valid UTF-8.
                result.push_str(s);
                break;
            }
            Err(e) => {
                let valid_up_to = e.valid_up_to();
                // Append the valid prefix.
                if valid_up_to > 0 {
                    // Safety: from_utf8 guarantees bytes[i..i+valid_up_to] is valid.
                    let valid = std::str::from_utf8(&bytes[i..i + valid_up_to]).unwrap();
                    result.push_str(valid);
                    i += valid_up_to;
                }
                // Now bytes[i] starts an invalid sequence. Emit Raku's marker
                // plus a visible hexadecimal payload for the single bad byte.
                let bad_byte = bytes[i];
                append_invalid_byte(&mut result, bad_byte);
                i += 1;
            }
        }
    }
    result
}

/// Encode a utf8-c8 string back to bytes.
///
/// A synthetic marker followed by `xNN` is emitted as the single byte `NN`.
/// All other characters are emitted as standard UTF-8.
pub fn encode_utf8_c8(s: &str) -> Vec<u8> {
    let mut result = Vec::new();
    let mut chars = s.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == SYNTHETIC_MARKER {
            let mut payload = chars.clone();
            if payload.next() == Some('x')
                && let (Some(hi), Some(lo)) = (payload.next(), payload.next())
                && let (Some(hi), Some(lo)) = (hi.to_digit(16), lo.to_digit(16))
            {
                result.push(((hi << 4) | lo) as u8);
                chars = payload;
                continue;
            }
        }

        let mut buf = [0u8; 4];
        result.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_simple_invalid_byte() {
        let input = vec![b'A', 0xFE, b'Z'];
        let decoded = decode_utf8_c8(&input);
        assert_eq!(decoded, "A\u{10FFFD}xFEZ");
        assert_eq!(decoded.chars().count(), 6);
        assert_eq!(decoded.chars().next().unwrap(), 'A');
        assert_eq!(decoded.chars().last().unwrap(), 'Z');
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }

    #[test]
    fn roundtrip_multiple_invalid_bytes() {
        let input = vec![b'A', 0xFE, 0xFD, b'Z'];
        let decoded = decode_utf8_c8(&input);
        assert_eq!(decoded, "A\u{10FFFD}xFE\u{10FFFD}xFDZ");
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }

    #[test]
    fn roundtrip_trailing_invalid() {
        let input = vec![b'A', b'B', 0xFC];
        let decoded = decode_utf8_c8(&input);
        assert_eq!(decoded, "AB\u{10FFFD}xFC");
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }

    #[test]
    fn valid_utf8_passthrough() {
        let input = "Hello, world!".as_bytes().to_vec();
        let decoded = decode_utf8_c8(&input);
        assert_eq!(decoded, "Hello, world!");
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }

    #[test]
    fn roundtrip_mixed_valid_invalid() {
        let input = vec![b'L', 0xE9, b'o', b'n'];
        let decoded = decode_utf8_c8(&input);
        assert_eq!(decoded, "L\u{10FFFD}xE9on");
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }
}
