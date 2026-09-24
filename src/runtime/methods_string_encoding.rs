use super::*;

impl Interpreter {
    pub(super) fn translate_newlines_for_encode(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\n', "\r"),
            NewlineMode::Crlf => input.replace('\n', "\r\n"),
        }
    }

    pub(crate) fn translate_newlines_for_decode(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\r', "\n"),
            NewlineMode::Crlf => input.replace("\r\n", "\n"),
        }
    }

    pub(super) fn encode_with_encoding_and_replacement(
        &self,
        input: &str,
        encoding_name: &str,
        replacement: Option<&str>,
    ) -> Result<Vec<u8>, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        if let Some(repl) = replacement {
            match encoding.as_str() {
                "ascii" => {
                    return Ok(input
                        .chars()
                        .flat_map(|ch| {
                            if (ch as u32) <= 0x7F {
                                vec![ch as u8]
                            } else {
                                repl.as_bytes().to_vec()
                            }
                        })
                        .collect());
                }
                "iso-8859-1" => {
                    return Ok(input
                        .chars()
                        .flat_map(|ch| {
                            if (ch as u32) <= 0xFF {
                                vec![ch as u8]
                            } else {
                                repl.as_bytes().to_vec()
                            }
                        })
                        .collect());
                }
                _ => {
                    if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                        // Pre-encode the replacement string in the target encoding
                        let repl_encoded = {
                            let (enc_bytes, _, _) = enc.encode(repl);
                            enc_bytes.into_owned()
                        };
                        let mut result = Vec::new();
                        for ch in input.chars() {
                            let s = ch.to_string();
                            let (encoded, _enc, had_errors) = enc.encode(&s);
                            if had_errors || !Self::char_is_encodable(ch, &encoded, enc) {
                                result.extend_from_slice(&repl_encoded);
                            } else {
                                result.extend_from_slice(&encoded);
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        self.encode_with_encoding(input, encoding_name)
    }

    /// Raku's human-readable name for an encoding, used in encode error
    /// messages (e.g. `ascii` -> `ASCII`, `windows-1252` -> `Windows-1252`).
    fn encoding_display_name(enc: &str) -> String {
        match enc {
            "ascii" => "ASCII".to_string(),
            "iso-8859-1" | "latin1" => "Latin-1".to_string(),
            _ if enc.starts_with("windows-") => format!("Windows-{}", &enc["windows-".len()..]),
            _ if enc.starts_with("utf") => enc.to_uppercase(),
            _ => enc.to_string(),
        }
    }

    /// The `X::AdHoc` message Raku raises when a codepoint cannot be encoded.
    fn encode_codepoint_error(enc: &str, ch: char) -> RuntimeError {
        RuntimeError::new(format!(
            "Error encoding {} string: could not encode codepoint {}",
            Self::encoding_display_name(enc),
            ch as u32
        ))
    }

    pub(super) fn encode_with_encoding(
        &self,
        input: &str,
        encoding_name: &str,
    ) -> Result<Vec<u8>, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "utf8-c8" => Ok(super::utf8_c8::encode_utf8_c8(input)),
            "ascii" => {
                for ch in input.chars() {
                    if (ch as u32) > 0x7F {
                        return Err(Self::encode_codepoint_error("ascii", ch));
                    }
                }
                Ok(input.bytes().collect())
            }
            "iso-8859-1" => {
                for ch in input.chars() {
                    if (ch as u32) > 0xFF {
                        return Err(Self::encode_codepoint_error("iso-8859-1", ch));
                    }
                }
                Ok(input.chars().map(|c| c as u8).collect())
            }
            "utf-16" | "utf-16le" => {
                Ok(input.encode_utf16().flat_map(|u| u.to_le_bytes()).collect())
            }
            "utf-16be" => Ok(input.encode_utf16().flat_map(|u| u.to_be_bytes()).collect()),
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    for ch in input.chars() {
                        let s = ch.to_string();
                        let (encoded, _, had_errors) = enc.encode(&s);
                        if had_errors || !Self::char_is_encodable(ch, &encoded, enc) {
                            return Err(Self::encode_codepoint_error(&encoding, ch));
                        }
                    }
                    if matches!(encoding.as_str(), "windows-1251" | "windows-1252") {
                        return Ok(Self::encode_single_byte_with_encoding_rs(input, enc));
                    }
                    let (encoded, _used_encoding, _had_errors) = enc.encode(input);
                    return Ok(encoded.into_owned());
                }
                Ok(input.as_bytes().to_vec())
            }
        }
    }

    /// Strict ASCII decode: every byte must be <= 0x7F, otherwise Raku throws
    /// `X::Str::Encode`-style "Will not decode invalid ASCII" rather than
    /// silently substituting a replacement character.
    /// Decode UTF-16 code units. Without a `replacement`, an unpaired surrogate
    /// is malformed and throws (matching Rakudo, which strictly rejects invalid
    /// UTF-16); with one, each invalid unit becomes the replacement string.
    fn decode_utf16_units(
        units: &[u16],
        replacement: Option<&str>,
    ) -> Result<String, RuntimeError> {
        match replacement {
            Some(repl) => Ok(char::decode_utf16(units.iter().copied())
                .map(|r| r.map(String::from).unwrap_or_else(|_| repl.to_string()))
                .collect()),
            None => char::decode_utf16(units.iter().copied())
                .map(|r| r.map_err(|_| ()))
                .collect::<Result<String, ()>>()
                .map_err(|()| {
                    RuntimeError::new("Malformed UTF-16 string: unpaired surrogate (line 1)")
                }),
        }
    }

    /// Decode `bytes` for an IO read, a socket, a `Proc` or `.decode`: the
    /// label is first resolved through the user encoding registry, then decoded
    /// by the one builtin decoder `Blob.decode` and `nqp::decode` use
    /// (`builtins::decode_bytes_with_encoding_label`, ADR-0118 §2.4), which
    /// also NFC-normalizes the result as rakudo's NFG strings are. This used
    /// to be a second copy of that decoder minus the normalization, so a
    /// slurped file and a decoded Blob of the same bytes differed.
    // Cost: O(n), n = bytes.
    pub(super) fn decode_with_encoding(
        &self,
        bytes: &[u8],
        encoding_name: &str,
    ) -> Result<String, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();
        if let Some(decoded) = crate::builtins::decode_bytes_with_encoding_label(bytes, &encoding) {
            return decoded;
        }
        // A label the builtin table does not know: an encoding_rs codec, else
        // lossy UTF-8 (an IO handle opened with an unrecognised name).
        if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
            let (decoded, _used_encoding, _had_errors) = enc.decode(bytes);
            return Ok(crate::builtins::nfc(decoded.into_owned()));
        }
        Ok(crate::builtins::nfc(
            String::from_utf8_lossy(bytes).into_owned(),
        ))
    }

    /// Decode bytes with an optional replacement string for unmappable/invalid
    /// sequences.  When `replacement` is `None` and the input contains errors,
    /// a `RuntimeError` is raised (Raku strict-decode semantics).  When
    /// `replacement` is `Some(r)`, every error sequence is replaced by `r`.
    pub(crate) fn decode_with_encoding_and_replacement(
        &self,
        bytes: &[u8],
        encoding_name: &str,
        replacement: Option<&str>,
    ) -> Result<String, RuntimeError> {
        // Without a replacement this IS the strict decode.
        let Some(repl) = replacement else {
            return self.decode_with_encoding(bytes, encoding_name);
        };
        self.decode_with_replacement(bytes, encoding_name, repl)
            .map(crate::builtins::nfc)
    }

    /// The lenient half of [`Self::decode_with_encoding_and_replacement`]:
    /// every invalid sequence becomes `replacement`. Normalized by the caller.
    fn decode_with_replacement(
        &self,
        bytes: &[u8],
        encoding_name: &str,
        repl: &str,
    ) -> Result<String, RuntimeError> {
        let replacement = Some(repl);
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "utf8-c8" => Ok(super::utf8_c8::decode_utf8_c8(bytes)),
            "ascii" => Ok(bytes
                .iter()
                .map(|b| {
                    if *b <= 0x7F {
                        (*b as char).to_string()
                    } else {
                        repl.to_string()
                    }
                })
                .collect()),
            "iso-8859-1" => Ok(bytes.iter().map(|b| *b as char).collect()),
            "utf-16" | "utf16" => {
                let (data, be) = if bytes.len() >= 2 && bytes[0] == 0xFE && bytes[1] == 0xFF {
                    (&bytes[2..], true)
                } else if bytes.len() >= 2 && bytes[0] == 0xFF && bytes[1] == 0xFE {
                    (&bytes[2..], false)
                } else {
                    (bytes, false)
                };
                if !data.len().is_multiple_of(2) {
                    return Err(RuntimeError::new(
                        "Malformed UTF-16 stream: odd byte length (terminated mid-code-unit)",
                    ));
                }
                let units: Vec<u16> = data
                    .chunks_exact(2)
                    .map(|c| {
                        if be {
                            u16::from_be_bytes([c[0], c[1]])
                        } else {
                            u16::from_le_bytes([c[0], c[1]])
                        }
                    })
                    .collect();
                Self::decode_utf16_units(&units, replacement)
            }
            "utf-16le" | "utf16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new(
                        "Malformed UTF-16 stream: odd byte length (terminated mid-code-unit)",
                    ));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_le_bytes([c[0], c[1]]))
                    .collect();
                Self::decode_utf16_units(&units, replacement)
            }
            "utf-16be" | "utf16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new(
                        "Malformed UTF-16BE stream: odd byte length (terminated mid-code-unit)",
                    ));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_be_bytes([c[0], c[1]]))
                    .collect();
                Self::decode_utf16_units(&units, replacement)
            }
            "utf-8" | "utf8" => match std::str::from_utf8(bytes) {
                Ok(s) => Ok(s.strip_prefix('\u{FEFF}').unwrap_or(s).to_string()),
                Err(_) => Ok(Self::decode_utf8_with_replacement(bytes, repl)),
            },
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    return Self::decode_encoding_rs(enc, bytes, replacement);
                }
                Ok(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }
}
