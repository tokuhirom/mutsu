use super::*;

impl Interpreter {
    /// Decode bytes using encoding_rs with proper error handling.
    /// When `replacement` is None, throws on any decode error.
    /// When `replacement` is Some(r), replaces error sequences with `r`.
    pub(crate) fn decode_encoding_rs(
        enc: &'static encoding_rs::Encoding,
        bytes: &[u8],
        replacement: Option<&str>,
    ) -> Result<String, RuntimeError> {
        // Fast path: if no replacement, try lossy decode and check for errors
        if replacement.is_none() {
            let (decoded, _used_encoding, had_errors) = enc.decode(bytes);
            if had_errors {
                return Err(RuntimeError::new(format!(
                    "Error decoding {} byte(s) as {}",
                    bytes.len(),
                    enc.name()
                )));
            }
            return Ok(decoded.into_owned());
        }
        let repl = replacement.unwrap();
        // Slow path: incremental decode to insert custom replacement strings
        let mut decoder = enc.new_decoder_without_bom_handling();
        let max_len = decoder
            .max_utf8_buffer_length_without_replacement(bytes.len())
            .unwrap_or(bytes.len() * 4);
        let mut output = String::with_capacity(max_len);
        let mut total_read = 0;
        loop {
            if total_read > bytes.len() {
                break;
            }
            let src = &bytes[total_read..];
            let is_last = true; // We provide all remaining input at once
            let needed = decoder
                .max_utf8_buffer_length_without_replacement(src.len())
                .unwrap_or(src.len() * 4 + 16);
            output.reserve(needed);
            let (result, read) =
                decoder.decode_to_string_without_replacement(src, &mut output, is_last);
            total_read += read;
            match result {
                encoding_rs::DecoderResult::InputEmpty => {
                    break;
                }
                encoding_rs::DecoderResult::Malformed(_, _) => {
                    output.push_str(repl);
                    // The malformed bytes are already accounted for in `read`
                    // (the decoder consumed them). Continue decoding the rest.
                }
                encoding_rs::DecoderResult::OutputFull => {
                    // Grow output and retry
                    output.reserve(needed + 1024);
                }
            }
        }
        Ok(output)
    }

    /// Decode UTF-8 bytes with a replacement string for invalid sequences.
    pub(super) fn decode_utf8_with_replacement(bytes: &[u8], replacement: &str) -> String {
        let mut result = String::new();
        let mut pos = 0;
        while pos < bytes.len() {
            match std::str::from_utf8(&bytes[pos..]) {
                Ok(s) => {
                    result.push_str(s);
                    break;
                }
                Err(e) => {
                    let valid_up_to = e.valid_up_to();
                    if valid_up_to > 0 {
                        result
                            .push_str(std::str::from_utf8(&bytes[pos..pos + valid_up_to]).unwrap());
                    }
                    result.push_str(replacement);
                    pos += valid_up_to + e.error_len().unwrap_or(1);
                }
            }
        }
        result
    }

    /// Check whether a character can be encoded in the given encoding following
    /// Raku's rules. This catches cases where `encoding_rs` (WHATWG) silently
    /// maps codepoints that Raku considers non-encodable:
    /// - GB18030: U+E000..U+E864 (PUA range excluded by the Chinese national
    ///   standard but mapped by WHATWG).
    /// - Other encodings: round-trip check (encode then decode must produce the
    ///   same character).
    pub(super) fn char_is_encodable(
        ch: char,
        encoded: &[u8],
        enc: &'static encoding_rs::Encoding,
    ) -> bool {
        // GB18030-specific: Raku rejects PUA U+E000..U+E864.
        if enc == encoding_rs::GB18030 {
            let cp = ch as u32;
            if (0xE000..=0xE864).contains(&cp) {
                return false;
            }
        }
        // General round-trip check for other mismatches.
        let (decoded, _, had_errors) = enc.decode(encoded);
        if had_errors {
            return false;
        }
        let mut chars = decoded.chars();
        matches!(chars.next(), Some(c) if c == ch) && chars.next().is_none()
    }

    pub(super) fn lookup_encoding_rs_codec(
        encoding: &str,
    ) -> Option<&'static encoding_rs::Encoding> {
        let label = match encoding {
            "windows-932" => "shift_jis",
            _ => encoding,
        };
        encoding_rs::Encoding::for_label(label.as_bytes())
    }

    pub(super) fn encode_single_byte_with_encoding_rs(
        input: &str,
        enc: &'static encoding_rs::Encoding,
    ) -> Vec<u8> {
        let mut reverse = HashMap::with_capacity(256);
        for b in 0u8..=255 {
            let one = [b];
            let (decoded, _used_encoding, _had_errors) = enc.decode(&one);
            let mut chars = decoded.chars();
            if let (Some(ch), None) = (chars.next(), chars.next()) {
                reverse.insert(ch, b);
            }
        }
        input
            .chars()
            .map(|ch| reverse.get(&ch).copied().unwrap_or(b'?'))
            .collect()
    }
}
