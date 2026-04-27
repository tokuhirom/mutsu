//! UTF-8 Clean-8 (utf8-c8) encoding support.
//!
//! utf8-c8 is Raku's encoding that allows round-tripping arbitrary byte
//! sequences through strings. Valid UTF-8 is decoded normally; invalid
//! bytes are represented as synthetic codepoints in the Supplementary
//! Private Use Area-A (U+F0000..U+F00FF), where the low 8 bits encode
//! the original byte value.
//!
//! When encoding back to utf8-c8, characters in U+F0000..U+F00FF are
//! emitted as the corresponding single byte, and all other characters
//! are emitted as standard UTF-8.

/// Base codepoint for synthetic byte representations.
const SYNTHETIC_BASE: u32 = 0xF0000;

/// Decode a byte slice using the utf8-c8 scheme.
///
/// Valid UTF-8 sequences are decoded normally. Each byte that is part of
/// an invalid sequence is individually mapped to the synthetic codepoint
/// U+F0000 + byte_value.
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
                // Now bytes[i] starts an invalid sequence. Emit one synthetic char
                // for the single bad byte.
                let bad_byte = bytes[i];
                let synthetic = char::from_u32(SYNTHETIC_BASE + bad_byte as u32)
                    .expect("synthetic codepoint must be valid");
                result.push(synthetic);
                i += 1;
            }
        }
    }
    result
}

/// Encode a utf8-c8 string back to bytes.
///
/// Characters in the synthetic range U+F0000..U+F00FF are emitted as
/// the single byte (codepoint - U+F0000). All other characters are
/// emitted as standard UTF-8.
pub fn encode_utf8_c8(s: &str) -> Vec<u8> {
    let mut result = Vec::new();
    for ch in s.chars() {
        let cp = ch as u32;
        if (SYNTHETIC_BASE..=SYNTHETIC_BASE + 0xFF).contains(&cp) {
            result.push((cp - SYNTHETIC_BASE) as u8);
        } else {
            let mut buf = [0u8; 4];
            let encoded = ch.encode_utf8(&mut buf);
            result.extend_from_slice(encoded.as_bytes());
        }
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
        assert_eq!(decoded.chars().count(), 3);
        assert_eq!(decoded.chars().next().unwrap(), 'A');
        assert_eq!(decoded.chars().last().unwrap(), 'Z');
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }

    #[test]
    fn roundtrip_multiple_invalid_bytes() {
        let input = vec![b'A', 0xFE, 0xFD, b'Z'];
        let decoded = decode_utf8_c8(&input);
        assert_eq!(decoded.chars().count(), 4);
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }

    #[test]
    fn roundtrip_trailing_invalid() {
        let input = vec![b'A', b'B', 0xFC];
        let decoded = decode_utf8_c8(&input);
        assert_eq!(decoded.chars().count(), 3);
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
        assert_eq!(decoded.chars().count(), 4);
        let encoded = encode_utf8_c8(&decoded);
        assert_eq!(encoded, input);
    }
}
