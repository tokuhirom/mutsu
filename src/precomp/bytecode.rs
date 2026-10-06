//! The compiled-bytecode section of the precompilation cache (ADR-11756).
//!
//! A module's compiled mainline is stored in `{hash}.code`, next to the AST
//! entry `{hash}.bin`. It is validated the same way the AST entry is (canonical
//! path, mtime, content hash, interpreter version) and, on top of that, by:
//!
//! - the **environment fingerprint**: the compiler's process-wide switches;
//! - the **compile context**: what `compile_block_raw` copies from the
//!   interpreter into the compiler (routine depth, packages, source file), plus
//!   a fingerprint of the guard statements `load_module_inner` splices into the
//!   AST, which depend on interpreter state;
//! - the **recorded inputs** ([`CompileInputs`]): re-asked by the caller at the
//!   point the compile would run, not here, because their answers depend on
//!   the parser state at that moment.
//!
//! The entry is self-contained (ADR-12026 §2.1): it also carries the parse
//! effects, the module's load facts and the AST the compile saw (prologue
//! ordered, without guards). A load that finds the entry reads none of the
//! AST cache and decodes the AST only if something asks for it, so the AST a
//! reader gets is always the one the cached code was compiled from.
//!
//! Layout: magic, metadata length, metadata, AST length, AST, compiled payload.

use super::{
    ParseEffects, cache_dir, content_hash, decode_config, interpreter_version, path_hash,
    source_mtime_nanos, temp_cache_path, warn_cache_unavailable,
};
use crate::compiler::compile_inputs::CompileInputs;
use crate::runtime::module_load_facts::ModuleLoadFacts;
use std::fs;
use std::path::{Path, PathBuf};

const CODE_MAGIC: &[u8; 4] = b"MTSC";

/// What makes one compile of a module mainline what it is, besides the AST
/// and the recorded inputs.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct CompileContext {
    pub(crate) environment: String,
    pub(crate) is_routine: bool,
    pub(crate) current_package: String,
    pub(crate) enclosing_package: Option<String>,
    /// Whether `$?DISTRIBUTION` has a value: without one the compiler bakes in
    /// `Nil`, with one an instance (which the codec refuses).
    pub(crate) has_distribution: bool,
    pub(crate) unit_file: Option<String>,
    /// [`crate::ast::stable_hash`] of the undeclared-routine guards spliced
    /// into the entry's AST before the compile. The rest of the AST is the
    /// entry's own.
    pub(crate) guards_fingerprint: u64,
}

/// The process-wide latches the compile left set, replayed on a hit.
#[derive(Debug, Clone, Copy, Default, serde::Serialize, serde::Deserialize)]
pub(crate) struct CompileLatches {
    pub(crate) reflective_name_access: bool,
    pub(crate) dispatcher: bool,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct CodeMetadata {
    source_path: String,
    mtime_nanos: u128,
    source_hash: u64,
    version: String,
    context: CompileContext,
    inputs: CompileInputs,
    latches: CompileLatches,
    effects: ParseEffects,
    facts: ModuleLoadFacts,
}

/// A cached module entry, not yet decoded beyond its metadata.
pub(crate) struct CodeEntry {
    pub(crate) context: CompileContext,
    pub(crate) inputs: CompileInputs,
    pub(crate) latches: CompileLatches,
    pub(crate) effects: ParseEffects,
    pub(crate) facts: ModuleLoadFacts,
    /// The AST, encoded with [`super::encode_stmts`].
    pub(crate) ast: Vec<u8>,
    /// The compiled mainline, encoded with [`crate::precomp_codec`].
    pub(crate) payload: Vec<u8>,
}

/// What [`save_code_entry`] stores.
pub(crate) struct NewCodeEntry<'a> {
    pub(crate) context: CompileContext,
    pub(crate) inputs: CompileInputs,
    pub(crate) latches: CompileLatches,
    pub(crate) effects: &'a ParseEffects,
    pub(crate) facts: &'a ModuleLoadFacts,
    pub(crate) ast: &'a [u8],
    pub(crate) payload: &'a [u8],
}

// Cost: O(1).
fn code_file(source_path: &Path) -> Option<(PathBuf, String)> {
    let canonical = source_path.canonicalize().ok()?;
    let dir = cache_dir().ok()?;
    let file = dir.join(format!("{}.code", path_hash(&canonical)));
    Some((file, canonical.to_string_lossy().into_owned()))
}

/// The cached entry of `source_path`, if one exists for exactly this source
/// text and interpreter build. Whether its compile fits the current context
/// is the caller's question.
// Cost: O(n), n = size of the entry (read; nothing is decoded but the metadata).
pub(crate) fn load_code_entry(source_path: &Path, source: &str) -> Option<CodeEntry> {
    let (file, canonical) = code_file(source_path)?;
    let data = fs::read(&file).ok()?;
    let rest = data.strip_prefix(CODE_MAGIC.as_slice())?;
    let (meta_bytes, rest) = split_section(rest)?;
    let (meta, _): (CodeMetadata, usize) =
        bincode::serde::decode_from_slice(meta_bytes, decode_config()).ok()?;
    let fresh = meta.source_path == canonical
        && meta.version == interpreter_version()
        && Some(meta.mtime_nanos) == source_mtime_nanos(source_path)
        && meta.source_hash == content_hash(source.as_bytes());
    if !fresh {
        let _ = fs::remove_file(&file);
        return None;
    }
    let (ast, payload) = split_section(rest)?;
    Some(CodeEntry {
        context: meta.context,
        inputs: meta.inputs,
        latches: meta.latches,
        effects: meta.effects,
        facts: meta.facts,
        ast: ast.to_vec(),
        payload: payload.to_vec(),
    })
}

/// A `u32` length-prefixed section, and what follows it.
// Cost: O(1).
fn split_section(data: &[u8]) -> Option<(&[u8], &[u8])> {
    let len = u32::from_le_bytes(data.get(..4)?.try_into().ok()?) as usize;
    let rest = &data[4..];
    Some((rest.get(..len)?, &rest[len..]))
}

/// Store the compile of `source_path`'s mainline. Best effort, like the AST
/// entry: any failure leaves no entry behind.
// Cost: O(n), n = size of the entry.
pub(crate) fn save_code_entry(source_path: &Path, source: &str, entry: NewCodeEntry<'_>) {
    let Some((file, canonical)) = code_file(source_path) else {
        return;
    };
    let Some(mtime_nanos) = source_mtime_nanos(source_path) else {
        return;
    };
    let meta = CodeMetadata {
        source_path: canonical,
        mtime_nanos,
        source_hash: content_hash(source.as_bytes()),
        version: interpreter_version(),
        context: entry.context,
        inputs: entry.inputs,
        latches: entry.latches,
        effects: entry.effects.clone(),
        facts: entry.facts.clone(),
    };
    let Ok(meta_bytes) = bincode::serde::encode_to_vec(&meta, bincode::config::standard()) else {
        return;
    };
    let mut data =
        Vec::with_capacity(12 + meta_bytes.len() + entry.ast.len() + entry.payload.len());
    data.extend_from_slice(CODE_MAGIC);
    data.extend_from_slice(&(meta_bytes.len() as u32).to_le_bytes());
    data.extend_from_slice(&meta_bytes);
    data.extend_from_slice(&(entry.ast.len() as u32).to_le_bytes());
    data.extend_from_slice(entry.ast);
    data.extend_from_slice(entry.payload);
    let tmp = temp_cache_path(&file);
    let written = fs::write(&tmp, &data).and_then(|()| fs::rename(&tmp, &file));
    if let Err(err) = written {
        warn_cache_unavailable(&err);
        let _ = fs::remove_file(&tmp);
    }
}
