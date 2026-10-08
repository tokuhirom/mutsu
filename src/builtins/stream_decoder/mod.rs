//! The streaming decoder (#11503): bytes in, chars and lines out, with
//! partial input held across calls. One state machine backs both layers that
//! expose it — the `nqp::decoder*` ops and the `Encoding::Decoder::Builtin`
//! methods Rakudo builds on them (`add-bytes`, `consume-line-chars`, ...) —
//! so the two cannot drift (ADR-0117's one-primitive rule).
//!
//! The state is MoarVM's three queues:
//!
//! * the **undecoded bytes** (`decoderbytesavailable` counts them, and
//!   `decodertakebytes` hands them out raw);
//! * the **pending** text: decoded, but held back because more input could
//!   still change it — a combining mark joining the last grapheme, or a `\n`
//!   turning a trailing `\r` into `\r\n` (see [`Hold`]);
//! * the **chars**: final text, ready to be taken.
//!
//! Decoding is lazy and stops as soon as the request is satisfied (a line
//! separator, `n` chars), as MoarVM's decoder does with its stopper, so the
//! bytes behind that point stay raw.

mod codec;

use codec::incomplete_error;
pub(crate) use codec::{Codec, Hold};

use crate::builtins::grapheme_index::Units;
use crate::value::BufBytes;
use crate::value::RuntimeError;

/// The line separators a decoder starts with (MoarVM's default).
pub(crate) const DEFAULT_LINE_SEPARATORS: [&str; 2] = ["\n", "\r\n"];

/// A decoder's fixed settings.
#[derive(Clone, Debug)]
pub(crate) struct DecoderConfig {
    pub codec: Codec,
    /// `\r\n` is handed out as `\n`.
    pub translate_nl: bool,
    pub line_separators: Vec<String>,
}

/// The decoded-text queues (the undecoded bytes live in a `Buf`).
#[derive(Clone, Debug, Default)]
pub(crate) struct TextQueues {
    /// Final text, ready to be taken.
    pub chars: String,
    /// Decoded text held back (see [`Hold`]).
    pub pending: String,
    /// A unit boundary in `chars` before which no line separator starts, so
    /// a line search arriving in many small chunks does not rescan.
    pub scanned: usize,
}

/// Decode `bytes` as a whole stream through the streaming decoder: what an
/// `IO::Handle` text read hands it (a record, or the rest of the file), so a
/// malformed or truncated input fails with the decoder's MoarVM-style error
/// rather than a one-shot decoder's own wording (#11783).
// Cost: O(n), n = bytes.
pub(crate) fn decode_stream(codec: Codec, bytes: &[u8]) -> Result<String, RuntimeError> {
    let cfg = DecoderConfig {
        codec,
        translate_nl: false,
        line_separators: Vec::new(),
    };
    let mut buf = BufBytes::new();
    buf.extend_from_slice(bytes);
    let mut q = TextQueues::default();
    StreamDecoder {
        cfg: &cfg,
        bytes: &mut buf,
        q: &mut q,
    }
    .take_all_chars()
}

/// One operation's view of a decoder.
pub(crate) struct StreamDecoder<'a> {
    pub cfg: &'a DecoderConfig,
    pub bytes: &'a mut BufBytes,
    pub q: &'a mut TextQueues,
}

impl StreamDecoder<'_> {
    /// `decoderempty`: nothing undecoded, pending or untaken.
    // Cost: O(1).
    pub(crate) fn is_empty(&self) -> bool {
        self.bytes.is_empty() && self.q.pending.is_empty() && self.q.chars.is_empty()
    }

    /// `decodertakebytes`: the next `n` undecoded bytes, or `None` (and
    /// nothing taken) when fewer are buffered.
    // Cost: O(n) (the remaining bytes are not moved).
    pub(crate) fn take_bytes(&mut self, n: usize) -> Option<Vec<u8>> {
        if self.bytes.len() < n {
            return None;
        }
        let taken = self.bytes[..n].to_vec();
        self.bytes.drop_front(n);
        Some(taken)
    }

    /// `decodertakeavailablechars`: every char that is final now.
    // Cost: O(b + c), b = undecoded bytes, c = chars taken.
    pub(crate) fn take_available_chars(&mut self) -> Result<String, RuntimeError> {
        self.decode_all()?;
        Ok(self.take_chars_prefix(self.q.chars.len(), self.q.chars.len()))
    }

    /// `decodertakeallchars`: everything, the end of the stream having been
    /// reached; bytes that do not form a whole character are an error.
    // Cost: O(b + c), b = undecoded bytes, c = chars taken.
    pub(crate) fn take_all_chars(&mut self) -> Result<String, RuntimeError> {
        self.finish()?;
        Ok(self.take_chars_prefix(self.q.chars.len(), self.q.chars.len()))
    }

    /// `decodertakechars` / `decodertakecharseof`: exactly `n` chars, or
    /// `None` when that many are not available. At the end of the stream
    /// (`eof`) fewer than `n` are handed out instead.
    // Cost: O(k + d), k = chars taken, d = bytes decoded to reach them.
    pub(crate) fn take_chars(
        &mut self,
        n: usize,
        eof: bool,
    ) -> Result<Option<String>, RuntimeError> {
        let mut have = Units::from(&self.q.chars, 0).take(n).count();
        while have < n {
            let before = self.q.chars.len();
            if !self.decode_one()? {
                break;
            }
            have += Units::from(&self.q.chars, before).take(n - have).count();
        }
        if have < n {
            if !eof {
                return Ok(None);
            }
            self.finish()?;
        }
        let cut = Units::from(&self.q.chars, 0)
            .nth(n)
            .map_or(self.q.chars.len(), |(at, _)| at);
        Ok(Some(self.take_chars_prefix(cut, cut)))
    }

    /// `decodertakeline`: the text up to and including the first line
    /// separator (excluding it when `chomp`). Without one, `None` — unless
    /// `eof`, when the rest of the stream is the last line.
    // Cost: O(l + d), l = chars of the line, d = bytes decoded to find it;
    // a line arriving in pieces is not rescanned (`TextQueues::scanned`).
    pub(crate) fn take_line(
        &mut self,
        chomp: bool,
        eof: bool,
    ) -> Result<Option<String>, RuntimeError> {
        loop {
            if let Some(line) = self.take_line_if_found(chomp) {
                return Ok(Some(line));
            }
            if !self.decode_one()? {
                break;
            }
        }
        if !eof {
            return Ok(None);
        }
        self.finish()?;
        if let Some(line) = self.take_line_if_found(chomp) {
            return Ok(Some(line));
        }
        Ok(Some(
            self.take_chars_prefix(self.q.chars.len(), self.q.chars.len()),
        ))
    }

    fn take_line_if_found(&mut self, chomp: bool) -> Option<String> {
        let (start, end) = self.find_separator()?;
        Some(self.take_chars_prefix(if chomp { start } else { end }, end))
    }

    /// Hand out `chars[..cut]` and drop `chars[..consumed]`.
    fn take_chars_prefix(&mut self, cut: usize, consumed: usize) -> String {
        let q = &mut *self.q;
        q.scanned = 0;
        if consumed == q.chars.len() {
            let mut all = std::mem::take(&mut q.chars);
            all.truncate(cut);
            return all;
        }
        let taken = q.chars[..cut].to_string();
        q.chars.drain(..consumed);
        taken
    }

    /// The earliest line separator in `chars` (the longest one at that
    /// position), as a byte range. A separator only matches whole units, so
    /// the `\n` separator does not split a `\r\n` grapheme.
    fn find_separator(&mut self) -> Option<(usize, usize)> {
        let chars = &self.q.chars;
        let longest = self.cfg.line_separators.iter().map(String::len).max()?;
        let mut rescan_from = None;
        for (at, _) in Units::from(chars, self.q.scanned) {
            let found = self
                .cfg
                .line_separators
                .iter()
                .filter(|sep| !sep.is_empty() && chars[at..].starts_with(sep.as_str()))
                .filter(|sep| ends_on_unit_boundary(chars, at, sep.len()))
                .map(String::len)
                .max();
            if let Some(len) = found {
                return Some((at, at + len));
            }
            // Text arriving later can only complete a separator that starts
            // too close to the end to have been ruled out already.
            if rescan_from.is_none() && chars.len() - at < longest {
                rescan_from = Some(at);
            }
        }
        self.q.scanned = rescan_from.unwrap_or(chars.len());
        None
    }

    /// Decode one unit into the pending text and release what became final.
    /// `false` when no whole unit is buffered.
    fn decode_one(&mut self) -> Result<bool, RuntimeError> {
        match self
            .cfg
            .codec
            .decode_unit(self.bytes, &mut self.q.pending)?
        {
            Some(n) => {
                self.bytes.drop_front(n);
                self.release(false);
                Ok(true)
            }
            None => Ok(false),
        }
    }

    /// Decode every whole unit buffered.
    fn decode_all(&mut self) -> Result<(), RuntimeError> {
        // What decoded before an error stays decoded.
        let (taken, err) = self.cfg.codec.decode_run(self.bytes, &mut self.q.pending);
        self.bytes.drop_front(taken);
        self.release(false);
        err.map_or(Ok(()), Err)
    }

    /// The end of the stream: decode everything and release the pending text.
    fn finish(&mut self) -> Result<(), RuntimeError> {
        self.decode_all()?;
        if !self.bytes.is_empty() {
            return Err(incomplete_error(self.bytes));
        }
        self.release(true);
        Ok(())
    }

    /// Move the pending text that is final (all of it when `flush`) to the
    /// chars queue, normalized.
    fn release(&mut self, flush: bool) {
        let pending = &self.q.pending;
        let keep_from = if flush {
            pending.len()
        } else {
            match self.cfg.codec.hold() {
                Hold::Nothing => pending.len(),
                Hold::CarriageReturn => pending.len() - usize::from(pending.ends_with('\r')),
                Hold::Grapheme => match Units::from(pending, 0).last() {
                    Some((at, unit)) if !ends_in_hard_break(unit) => at,
                    _ => pending.len(),
                },
            }
        };
        if keep_from == 0 {
            return;
        }
        let rest = self.q.pending.split_off(keep_from);
        let mut text = std::mem::replace(&mut self.q.pending, rest);
        if self.cfg.codec.normalizes() {
            text = crate::ucd::normalize::nfc(text);
        }
        if self.cfg.translate_nl && text.contains("\r\n") {
            text = text.replace("\r\n", "\n");
        }
        self.q.chars.push_str(&text);
    }
}

/// A unit nothing can extend: it ends in `\n` or in a control other than
/// `\r` (UAX #29 GB4 breaks after them; `\r` waits for a possible `\n`).
fn ends_in_hard_break(unit: &str) -> bool {
    unit.chars()
        .next_back()
        .is_some_and(|c| c == '\n' || (c.is_control() && c != '\r'))
}

/// Whether `s[at..at + len]` ends exactly where a unit ends (`at` being a
/// unit start).
fn ends_on_unit_boundary(s: &str, at: usize, len: usize) -> bool {
    let mut end = at;
    for (start, unit) in Units::from(s, at) {
        end = start + unit.len();
        if end >= at + len {
            break;
        }
    }
    end == at + len
}

#[cfg(test)]
#[path = "tests.rs"]
mod tests;
