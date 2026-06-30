//! Interpreter-side handle character reading and UTF-8/UTF-16 decoding.
use super::*;

impl Interpreter {
    pub(crate) fn read_utf8_char<R: Read>(reader: &mut R) -> Result<Option<String>, RuntimeError> {
        let mut first = [0u8; 1];
        let n = reader
            .read(&mut first)
            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
        if n == 0 {
            return Ok(None);
        }

        let expected_len = match first[0] {
            0x00..=0x7F => 1usize,
            0xC0..=0xDF => 2usize,
            0xE0..=0xEF => 3usize,
            0xF0..=0xF7 => 4usize,
            _ => 1usize,
        };

        let mut bytes = vec![first[0]];
        if expected_len > 1 {
            let mut rest = vec![0u8; expected_len - 1];
            let mut total = 0usize;
            while total < rest.len() {
                let read = reader
                    .read(&mut rest[total..])
                    .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                if read == 0 {
                    break;
                }
                total += read;
            }
            bytes.extend_from_slice(&rest[..total]);
        }

        Ok(Some(String::from_utf8_lossy(&bytes).to_string()))
    }

    /// Read one character from a `utf8-c8` (UTF-8 Clean-8) stream. Valid UTF-8
    /// decodes normally; a byte that cannot start/complete a valid sequence
    /// becomes one synthetic grapheme (`decode_utf8_c8`). Continuation bytes of a
    /// rejected sequence are rewound (Seek) so the next read re-examines them.
    fn read_utf8_c8_char<R: Read + std::io::Seek>(
        reader: &mut R,
    ) -> Result<Option<String>, RuntimeError> {
        let io_err = |err| RuntimeError::new(format!("Failed to read: {}", err));
        let mut first = [0u8; 1];
        let n = reader.read(&mut first).map_err(io_err)?;
        if n == 0 {
            return Ok(None);
        }
        let lead = first[0];
        let expected_len = match lead {
            0x00..=0x7F => return Ok(Some((lead as char).to_string())),
            0xC2..=0xDF => 2usize,
            0xE0..=0xEF => 3usize,
            0xF0..=0xF4 => 4usize,
            // Invalid lead byte (lone continuation, 0xC0/0xC1, 0xF5+): one
            // synthetic grapheme for this single byte.
            _ => return Ok(Some(crate::runtime::utf8_c8::decode_utf8_c8(&[lead]))),
        };
        let mut rest = vec![0u8; expected_len - 1];
        let mut total = 0usize;
        while total < rest.len() {
            let r = reader.read(&mut rest[total..]).map_err(io_err)?;
            if r == 0 {
                break;
            }
            total += r;
        }
        let mut bytes = vec![lead];
        bytes.extend_from_slice(&rest[..total]);
        match std::str::from_utf8(&bytes) {
            Ok(s) => Ok(Some(s.to_string())),
            Err(_) => {
                // Invalid sequence: emit a synthetic grapheme for the lead byte
                // and rewind the continuation bytes for the next read.
                reader
                    .seek(std::io::SeekFrom::Current(-(total as i64)))
                    .map_err(io_err)?;
                Ok(Some(crate::runtime::utf8_c8::decode_utf8_c8(&[lead])))
            }
        }
    }

    /// Read one character from a UTF-16 stream.
    /// `big_endian` controls byte order. Handles surrogate pairs.
    fn read_utf16_char<R: Read>(
        reader: &mut R,
        big_endian: bool,
    ) -> Result<Option<String>, RuntimeError> {
        let mut buf = [0u8; 2];
        let n = reader
            .read(&mut buf)
            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
        if n == 0 {
            return Ok(None);
        }
        if n < 2 {
            // Incomplete code unit — replacement char
            return Ok(Some("\u{FFFD}".to_string()));
        }
        let unit = if big_endian {
            u16::from_be_bytes(buf)
        } else {
            u16::from_le_bytes(buf)
        };
        // Check for surrogate pair
        if (0xD800..=0xDBFF).contains(&unit) {
            // High surrogate — read low surrogate
            let mut buf2 = [0u8; 2];
            let n2 = reader
                .read(&mut buf2)
                .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
            if n2 < 2 {
                return Ok(Some("\u{FFFD}".to_string()));
            }
            let low = if big_endian {
                u16::from_be_bytes(buf2)
            } else {
                u16::from_le_bytes(buf2)
            };
            let codepoint = 0x10000 + ((unit as u32 - 0xD800) << 10) + (low as u32 - 0xDC00);
            match char::from_u32(codepoint) {
                Some(ch) => Ok(Some(ch.to_string())),
                None => Ok(Some("\u{FFFD}".to_string())),
            }
        } else {
            match char::from_u32(unit as u32) {
                Some(ch) => Ok(Some(ch.to_string())),
                None => Ok(Some("\u{FFFD}".to_string())),
            }
        }
    }

    pub(super) fn read_chars_from_handle_value(
        &mut self,
        handle_value: &Value,
        count: Option<usize>,
    ) -> Result<String, RuntimeError> {
        self.with_handle_mut(handle_value, |state| {
            if state.closed {
                return Err(RuntimeError::io_closed("handle operation"));
            }
            state.read_attempted = true;
            let encoding = state.encoding.to_lowercase();
            // Determine if this is a utf16 stream and its byte order
            let utf16_auto = matches!(encoding.as_str(), "utf-16" | "utf16");
            let utf16_mode = match encoding.as_str() {
                "utf-16be" | "utf16be" => Some(true),  // big endian
                "utf-16le" | "utf16le" => Some(false), // little endian
                "utf-16" | "utf16" => {
                    // Use previously detected endianness, or default to native
                    Some(
                        state
                            .utf16_detected_be
                            .unwrap_or(cfg!(target_endian = "big")),
                    )
                }
                _ => None,
            };
            match state.target {
                IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                    Err(RuntimeError::new("Handle not readable"))
                }
                IoHandleTarget::Stdin => {
                    let mut stdin = std::io::stdin().lock();
                    if let Some(limit) = count {
                        if limit == 0 {
                            return Ok(String::new());
                        }
                        let mut out = String::new();
                        for _ in 0..limit {
                            let Some(ch) = Self::read_utf8_char(&mut stdin)? else {
                                break;
                            };
                            out.push_str(&ch);
                        }
                        Ok(out)
                    } else {
                        let mut bytes = Vec::new();
                        stdin
                            .read_to_end(&mut bytes)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        Ok(String::from_utf8_lossy(&bytes).to_string())
                    }
                }
                IoHandleTarget::ArgFiles => Ok(String::new()),
                IoHandleTarget::File => {
                    let file = state
                        .file
                        .as_mut()
                        .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                    if let Some(mut big_endian) = utf16_mode {
                        // For utf16 auto-detect: detect BOM on first read
                        if utf16_auto && state.utf16_detected_be.is_none() {
                            let mut bom_buf = [0u8; 2];
                            let n = file.read(&mut bom_buf).map_err(|err| {
                                RuntimeError::new(format!("Failed to read: {}", err))
                            })?;
                            if n >= 2 {
                                if bom_buf[0] == 0xFE && bom_buf[1] == 0xFF {
                                    big_endian = true;
                                    state.utf16_detected_be = Some(true);
                                    // BOM consumed, don't include in output
                                } else if bom_buf[0] == 0xFF && bom_buf[1] == 0xFE {
                                    big_endian = false;
                                    state.utf16_detected_be = Some(false);
                                    // BOM consumed, don't include in output
                                } else {
                                    // No BOM, seek back
                                    state.utf16_detected_be = Some(cfg!(target_endian = "big"));
                                    use std::io::Seek;
                                    let _ = file.seek(std::io::SeekFrom::Current(-2));
                                }
                            }
                        }
                        if let Some(limit) = count {
                            if limit == 0 {
                                return Ok(String::new());
                            }
                            let mut out = String::new();
                            for _ in 0..limit {
                                let Some(ch) = Self::read_utf16_char(file, big_endian)? else {
                                    break;
                                };
                                out.push_str(&ch);
                            }
                            Ok(out)
                        } else {
                            let mut bytes = Vec::new();
                            file.read_to_end(&mut bytes).map_err(|err| {
                                RuntimeError::new(format!("Failed to read: {}", err))
                            })?;
                            // Decode all bytes using the utf16 decoder
                            let units: Vec<u16> = bytes
                                .chunks_exact(2)
                                .map(|c| {
                                    if big_endian {
                                        u16::from_be_bytes([c[0], c[1]])
                                    } else {
                                        u16::from_le_bytes([c[0], c[1]])
                                    }
                                })
                                .collect();
                            Ok(String::from_utf16_lossy(&units))
                        }
                    } else if let Some(limit) = count {
                        if limit == 0 {
                            return Ok(String::new());
                        }
                        let is_c8 = encoding == "utf8-c8";
                        let mut out = String::new();
                        for _ in 0..limit {
                            let ch = if is_c8 {
                                Self::read_utf8_c8_char(file)?
                            } else {
                                Self::read_utf8_char(file)?
                            };
                            let Some(ch) = ch else {
                                break;
                            };
                            out.push_str(&ch);
                        }
                        Ok(out)
                    } else if encoding == "utf8-c8" {
                        let mut bytes = Vec::new();
                        file.read_to_end(&mut bytes)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        Ok(crate::runtime::utf8_c8::decode_utf8_c8(&bytes))
                    } else {
                        let mut bytes = Vec::new();
                        file.read_to_end(&mut bytes)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        Ok(String::from_utf8_lossy(&bytes).to_string())
                    }
                }
                IoHandleTarget::Socket => {
                    let sock = state
                        .socket
                        .as_mut()
                        .ok_or_else(|| RuntimeError::new("Socket not connected"))?;
                    if let Some(limit) = count {
                        if limit == 0 {
                            return Ok(String::new());
                        }
                        let mut out = String::new();
                        for _ in 0..limit {
                            let Some(ch) = Self::read_utf8_char(sock)? else {
                                break;
                            };
                            out.push_str(&ch);
                        }
                        Ok(out)
                    } else {
                        let mut bytes = Vec::new();
                        sock.read_to_end(&mut bytes)
                            .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                        Ok(String::from_utf8_lossy(&bytes).to_string())
                    }
                }
            }
        })
    }
}
