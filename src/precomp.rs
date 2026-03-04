//! Module precompilation cache.
//!
//! Caches the parsed AST (`Vec<Stmt>`) for loaded modules on disk so that
//! subsequent runs can skip the parse step when the source has not changed.
//!
//! ## Cache layout
//!
//! Cache files are stored under `$HOME/.cache/mutsu/precomp/` (or a
//! platform-appropriate cache directory). Each cached module is stored as
//! `{cache_dir}/{hex_hash}.bin` where `hex_hash` is a hash of the canonical
//! source path.
//!
//! ## Cache key
//!
//! A cache entry is valid when ALL of the following match:
//! - The source file modification time matches the stored mtime
//! - The interpreter version matches the stored version stamp
//!
//! ## Serialization
//!
//! Uses `bincode` to serialize `Vec<Stmt>` (the parsed AST). All AST types
//! (`Stmt`, `Expr`, `ParamDef`, etc.) derive `serde::Serialize` and
//! `serde::Deserialize`. The `Value` enum uses custom serde that supports
//! only the subset of variants that can appear in AST literals.

use crate::ast::Stmt;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Magic bytes for the cache format.
const CACHE_MAGIC: &[u8; 4] = b"MTSU";

/// The interpreter version stamp embedded in cache files.
/// Cache is invalidated whenever this changes.
fn interpreter_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

/// Compute a deterministic hash of a canonical file path for use as cache filename.
fn path_hash(path: &Path) -> String {
    let mut hasher = DefaultHasher::new();
    path.to_string_lossy().hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

/// Get the cache directory, creating it if needed.
/// Returns None if the cache directory cannot be determined or created.
fn cache_dir() -> Option<PathBuf> {
    let base = if let Ok(xdg) = std::env::var("XDG_CACHE_HOME") {
        PathBuf::from(xdg)
    } else if let Ok(home) = std::env::var("HOME") {
        PathBuf::from(home).join(".cache")
    } else {
        return None;
    };
    let dir = base.join("mutsu").join("precomp");
    if !dir.exists() {
        fs::create_dir_all(&dir).ok()?;
    }
    Some(dir)
}

/// Metadata stored alongside the cached AST.
#[derive(serde::Serialize, serde::Deserialize)]
struct CacheMetadata {
    /// Source file modification time as seconds since UNIX epoch.
    mtime_secs: u64,
    /// Interpreter version at the time of caching.
    version: String,
}

/// Try to load a cached AST for the given source file.
///
/// Returns `Some(stmts)` if a valid cache entry exists, `None` otherwise.
pub(crate) fn load_cached_ast(source_path: &Path) -> Option<Vec<Stmt>> {
    let canonical = source_path.canonicalize().ok()?;
    let dir = cache_dir()?;
    let hash = path_hash(&canonical);
    let cache_file = dir.join(format!("{}.bin", hash));

    if !cache_file.exists() {
        return None;
    }

    let data = fs::read(&cache_file).ok()?;
    if data.len() < 4 {
        return None;
    }

    // Check magic bytes
    if &data[0..4] != CACHE_MAGIC {
        return None;
    }

    // Deserialize: metadata length (u32) + metadata + ast
    let rest = &data[4..];
    if rest.len() < 4 {
        return None;
    }
    let meta_len = u32::from_le_bytes([rest[0], rest[1], rest[2], rest[3]]) as usize;
    let rest = &rest[4..];
    if rest.len() < meta_len {
        return None;
    }

    let meta: CacheMetadata = bincode::deserialize(&rest[..meta_len]).ok()?;
    let ast_data = &rest[meta_len..];

    // Validate version
    if meta.version != interpreter_version() {
        // Version mismatch — remove stale cache
        let _ = fs::remove_file(&cache_file);
        return None;
    }

    // Validate mtime
    let source_mtime = source_mtime_secs(source_path)?;
    if meta.mtime_secs != source_mtime {
        // Source changed — remove stale cache
        let _ = fs::remove_file(&cache_file);
        return None;
    }

    // Check format version (stored right after magic)
    // Actually, let me restructure: magic(4) + format_version(4) + meta_len(4) + meta + ast
    // But we already wrote the format above without format_version. Let me just embed
    // format_version in the metadata for simplicity.

    bincode::deserialize(ast_data).ok()
}

/// Save a parsed AST to the cache for the given source file.
///
/// Errors are silently ignored (cache is best-effort).
pub(crate) fn save_cached_ast(source_path: &Path, stmts: &[Stmt]) {
    let Some(canonical) = source_path.canonicalize().ok() else {
        return;
    };
    let Some(dir) = cache_dir() else {
        return;
    };
    let Some(source_mtime) = source_mtime_secs(source_path) else {
        return;
    };

    let hash = path_hash(&canonical);
    let cache_file = dir.join(format!("{}.bin", hash));

    let meta = CacheMetadata {
        mtime_secs: source_mtime,
        version: interpreter_version().to_string(),
    };

    let Ok(meta_bytes) = bincode::serialize(&meta) else {
        return;
    };
    let Ok(ast_bytes) = bincode::serialize(stmts) else {
        return;
    };

    let meta_len = meta_bytes.len() as u32;
    let mut data = Vec::with_capacity(4 + 4 + meta_bytes.len() + ast_bytes.len());
    data.extend_from_slice(CACHE_MAGIC);
    data.extend_from_slice(&meta_len.to_le_bytes());
    data.extend_from_slice(&meta_bytes);
    data.extend_from_slice(&ast_bytes);

    // Write atomically via temp file + rename
    let tmp_file = cache_file.with_extension("tmp");
    if fs::write(&tmp_file, &data).is_ok() {
        let _ = fs::rename(&tmp_file, &cache_file);
    }
}

/// Clear all cached precompilation files.
#[allow(dead_code)]
pub(crate) fn clear_cache() {
    if let Some(dir) = cache_dir() {
        let _ = fs::remove_dir_all(&dir);
    }
}

/// Get the modification time of a file as seconds since UNIX epoch.
fn source_mtime_secs(path: &Path) -> Option<u64> {
    let metadata = fs::metadata(path).ok()?;
    let modified = metadata.modified().ok()?;
    let duration = modified.duration_since(SystemTime::UNIX_EPOCH).ok()?;
    Some(duration.as_secs())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Stmt};
    use crate::value::Value;
    use std::io::Write;

    #[test]
    fn roundtrip_simple_ast() {
        let stmts = vec![
            Stmt::Say(vec![Expr::Literal(Value::Int(42))]),
            Stmt::Expr(Expr::Literal(Value::str("hello".to_string()))),
        ];

        // Create a temporary source file
        let dir = tempdir("roundtrip");
        let source = dir.join("test.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 42;").unwrap();
        }

        // Save and load
        save_cached_ast(&source, &stmts);
        let loaded = load_cached_ast(&source);
        assert!(loaded.is_some(), "cache should return Some");
        let loaded = loaded.unwrap();
        assert_eq!(loaded.len(), 2);

        // Verify round-trip preserves structure
        assert!(matches!(&loaded[0], Stmt::Say(args) if args.len() == 1));
        assert!(matches!(
            &loaded[1],
            Stmt::Expr(Expr::Literal(Value::Str(_)))
        ));

        // Clean up
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn cache_invalidated_on_mtime_change() {
        let stmts = vec![Stmt::Say(vec![Expr::Literal(Value::Int(1))])];

        let dir = tempdir("mtime");
        let source = dir.join("test2.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 1;").unwrap();
        }

        save_cached_ast(&source, &stmts);
        assert!(load_cached_ast(&source).is_some());

        // Touch the file (update mtime)
        std::thread::sleep(std::time::Duration::from_secs(2));
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 2;").unwrap();
        }

        // Cache should now be invalid
        assert!(load_cached_ast(&source).is_none());

        let _ = fs::remove_dir_all(&dir);
    }

    fn tempdir(suffix: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        path.push(format!(
            "mutsu-precomp-test-{}-{}",
            std::process::id(),
            suffix
        ));
        let _ = fs::create_dir_all(&path);
        path
    }
}
