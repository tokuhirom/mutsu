//! The BEGIN effect log of a precompiled module (ADR-18012 §2.1).
//!
//! A module's compiled entry (`{hash}.code`, [`super::bytecode`]) is written
//! when the module is compiled, before its mainline runs, so it cannot carry
//! what the mainline's BEGIN-time phase leaves behind. That log goes in
//! `{hash}.begin`, written after the mainline has run. It is bound to the exact
//! compiled entry it was recorded with: it carries that entry's checksum and
//! the interpreter version, and is ignored (and removed) when either differs.
//! A recompile writes a new `.code` entry with a new checksum, so a stale log
//! can never be replayed against it.
//!
//! The file is read and written under the same private-file rules as the
//! compiled entry (ADR-11756; `O_NOFOLLOW`, owner-only). A malformed,
//! truncated or damaged file is a miss, never trusted in part (ADR-18012
//! §2.5): the body carries its own checksum, and decoding is bounded by the
//! file size.
//!
//! Layout: magic, the `.code` entry's checksum (u64 LE), the version length
//! (u32 LE) and text, the body checksum (u64 LE), the body.

use super::{cache_dir, interpreter_version, path_hash, private_files, warn_cache_unavailable};
use std::fs;
use std::path::{Path, PathBuf};

const BEGIN_MAGIC: &[u8; 4] = b"MTSB";

// Cost: O(1) (a canonicalize and a hash of the path).
fn begin_file(source_path: &Path) -> Option<PathBuf> {
    let canonical = source_path.canonicalize().ok()?;
    let dir = cache_dir().ok()?;
    Some(dir.join(format!("{}.begin", path_hash(&canonical))))
}

// Cost: O(n), n = body bytes.
fn body_checksum(body: &[u8]) -> u64 {
    use std::hash::Hasher;
    let mut hasher = rustc_hash::FxHasher::default();
    hasher.write_usize(body.len());
    hasher.write(body);
    hasher.finish()
}

/// The log body recorded for `source_path` against the compiled entry whose
/// checksum is `entry_checksum`, if there is an intact one.
// Cost: O(n), n = size of the file.
pub(crate) fn load_begin_log(source_path: &Path, entry_checksum: u64) -> Option<Vec<u8>> {
    let file = begin_file(source_path)?;
    let data = private_files::read_private(&file).ok()?;
    let parsed = parse(&data, entry_checksum);
    if parsed.is_none() {
        let _ = fs::remove_file(&file);
    }
    parsed.map(<[u8]>::to_vec)
}

// Cost: O(n), n = size of `data`.
fn parse(data: &[u8], entry_checksum: u64) -> Option<&[u8]> {
    let rest = data.strip_prefix(BEGIN_MAGIC.as_slice())?;
    let (checksum, rest) = take_u64(rest)?;
    if checksum != entry_checksum {
        return None;
    }
    let len = u32::from_le_bytes(rest.get(..4)?.try_into().ok()?) as usize;
    let rest = rest.get(4..)?;
    let version = std::str::from_utf8(rest.get(..len)?).ok()?;
    if version != interpreter_version() {
        return None;
    }
    let (sum, body) = take_u64(rest.get(len..)?)?;
    (body_checksum(body) == sum).then_some(body)
}

// Cost: O(1).
fn take_u64(data: &[u8]) -> Option<(u64, &[u8])> {
    let value = u64::from_le_bytes(data.get(..8)?.try_into().ok()?);
    Some((value, data.get(8..)?))
}

/// Store `body` as the log of `source_path`'s compiled entry `entry_checksum`.
/// Best effort, like the compiled entry: a failure leaves no log, and the next
/// load records one again.
// Cost: O(n), n = body bytes.
pub(crate) fn save_begin_log(source_path: &Path, entry_checksum: u64, body: &[u8]) {
    let Some(file) = begin_file(source_path) else {
        return;
    };
    let version = interpreter_version();
    let mut data = Vec::with_capacity(28 + version.len() + body.len());
    data.extend_from_slice(BEGIN_MAGIC);
    data.extend_from_slice(&entry_checksum.to_le_bytes());
    data.extend_from_slice(&(version.len() as u32).to_le_bytes());
    data.extend_from_slice(version.as_bytes());
    data.extend_from_slice(&body_checksum(body).to_le_bytes());
    data.extend_from_slice(body);
    if let Err(err) = private_files::write_private(&file, &data) {
        warn_cache_unavailable(&err);
    }
}

#[cfg(test)]
mod tests {
    use super::{BEGIN_MAGIC, body_checksum, interpreter_version, parse};

    fn file(checksum: u64, body: &[u8]) -> Vec<u8> {
        let version = interpreter_version();
        let mut data = BEGIN_MAGIC.to_vec();
        data.extend_from_slice(&checksum.to_le_bytes());
        data.extend_from_slice(&(version.len() as u32).to_le_bytes());
        data.extend_from_slice(version.as_bytes());
        data.extend_from_slice(&body_checksum(body).to_le_bytes());
        data.extend_from_slice(body);
        data
    }

    #[test]
    fn an_intact_log_for_its_entry_parses() {
        assert_eq!(parse(&file(7, b"log"), 7), Some(&b"log"[..]));
    }

    #[test]
    fn a_log_of_another_entry_or_a_damaged_one_is_a_miss() {
        assert_eq!(parse(&file(7, b"log"), 8), None);
        let mut damaged = file(7, b"log");
        if let Some(last) = damaged.last_mut() {
            *last ^= 1;
        }
        assert_eq!(parse(&damaged, 7), None);
        let whole = file(7, b"log");
        assert_eq!(parse(&whole[..whole.len() - 1], 7), None);
        assert_eq!(parse(b"MTSB", 7), None);
    }
}
