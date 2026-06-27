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
                        return Err(RuntimeError::new(format!(
                            "Error encoding ascii string: character '{}' (U+{:04X}) is not representable",
                            ch, ch as u32
                        )));
                    }
                }
                Ok(input.bytes().collect())
            }
            "iso-8859-1" => {
                for ch in input.chars() {
                    if (ch as u32) > 0xFF {
                        return Err(RuntimeError::new(format!(
                            "Error encoding iso-8859-1 string: character '{}' (U+{:04X}) is not representable",
                            ch, ch as u32
                        )));
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
                            return Err(RuntimeError::new(format!(
                                "Error encoding {} string: could not encode codepoint 0x{:x}",
                                encoding, ch as u32
                            )));
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
    fn decode_ascii_strict(bytes: &[u8]) -> Result<String, RuntimeError> {
        if let Some(b) = bytes.iter().find(|b| **b > 0x7F) {
            return Err(RuntimeError::new(format!(
                "Will not decode invalid ASCII (code point ({}) > 127 found)",
                b
            )));
        }
        Ok(bytes.iter().map(|b| *b as char).collect())
    }

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

        match encoding.as_str() {
            "utf8-c8" => Ok(super::utf8_c8::decode_utf8_c8(bytes)),
            "ascii" => Self::decode_ascii_strict(bytes),
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
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
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
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16le" | "utf16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_le_bytes([c[0], c[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16be" | "utf16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16be byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_be_bytes([c[0], c[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-8" | "utf8" => match std::str::from_utf8(bytes) {
                Ok(s) => Ok(s.strip_prefix('\u{FEFF}').unwrap_or(s).to_string()),
                Err(e) => {
                    let vup = e.valid_up_to();
                    if e.error_len().is_none() {
                        Err(RuntimeError::new(
                            "Malformed termination of UTF-8 string".to_string(),
                        ))
                    } else {
                        let pfx = std::str::from_utf8(&bytes[..vup]).unwrap_or("");
                        let (mut line, mut col) = (1usize, 1usize);
                        for ch in pfx.chars() {
                            if ch == '\n' {
                                line += 1;
                                col = 1;
                            } else {
                                col += 1;
                            }
                        }
                        let s = if vup > 0 { vup - 1 } else { 0 };
                        let end = (vup + 2).min(bytes.len());
                        let near: Vec<String> =
                            bytes[s..end].iter().map(|b| format!("{:02x}", b)).collect();
                        Err(RuntimeError::new(format!(
                            "Malformed UTF-8 near bytes {} at line {} col {}",
                            near.join(" "),
                            line,
                            col
                        )))
                    }
                }
            },
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    let (decoded, _used_encoding, _had_errors) = enc.decode(bytes);
                    return Ok(decoded.into_owned());
                }
                Ok(String::from_utf8_lossy(bytes).into_owned())
            }
        }
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
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "utf8-c8" => Ok(super::utf8_c8::decode_utf8_c8(bytes)),
            "ascii" => match replacement {
                Some(repl) => Ok(bytes
                    .iter()
                    .map(|b| {
                        if *b <= 0x7F {
                            (*b as char).to_string()
                        } else {
                            repl.to_string()
                        }
                    })
                    .collect()),
                None => Self::decode_ascii_strict(bytes),
            },
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
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
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
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16le" | "utf16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_le_bytes([c[0], c[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16be" | "utf16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16be byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|c| u16::from_be_bytes([c[0], c[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-8" | "utf8" => match std::str::from_utf8(bytes) {
                Ok(s) => Ok(s.strip_prefix('\u{FEFF}').unwrap_or(s).to_string()),
                Err(e) => {
                    if let Some(repl) = replacement {
                        return Ok(Self::decode_utf8_with_replacement(bytes, repl));
                    }
                    let vup = e.valid_up_to();
                    if e.error_len().is_none() {
                        Err(RuntimeError::new(
                            "Malformed termination of UTF-8 string".to_string(),
                        ))
                    } else {
                        let pfx = std::str::from_utf8(&bytes[..vup]).unwrap_or("");
                        let (mut line, mut col) = (1usize, 1usize);
                        for ch in pfx.chars() {
                            if ch == '\n' {
                                line += 1;
                                col = 1;
                            } else {
                                col += 1;
                            }
                        }
                        let s = if vup > 0 { vup - 1 } else { 0 };
                        let end = (vup + 2).min(bytes.len());
                        let near: Vec<String> =
                            bytes[s..end].iter().map(|b| format!("{:02x}", b)).collect();
                        Err(RuntimeError::new(format!(
                            "Malformed UTF-8 near bytes {} at line {} col {}",
                            near.join(" "),
                            line,
                            col
                        )))
                    }
                }
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
