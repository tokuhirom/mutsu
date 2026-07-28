//! Module precompilation cache.
//!
//! Caches the parsed AST (`Vec<Stmt>`) for loaded modules on disk so that
//! subsequent runs can skip the parse step when the source has not changed.
//!
//! ## Parsing is not a pure function
//!
//! Skipping the parse is only sound if parsing is `source -> AST`. It is not:
//! the parser also writes thread-local state that the rest of the runtime reads
//! afterwards. A cache hit performs none of those writes, so anything that
//! depends on them behaved differently depending on whether the cache happened
//! to be warm — the same program, two different results.
//!
//! Two such effects are proven and are therefore captured in the cache entry
//! (`ParseEffects`) and replayed on a hit:
//!
//! - **the language revision** the module's `use vX` selected. Without the
//!   replay, code running while the module's mainline executes saw the
//!   *importer's* revision (this made `roast/S14-roles/versioning.t` pass on a
//!   cold cache and fail on every run after it).
//! - **parse warnings**, which were emitted on the first run and then silently
//!   vanished on every subsequent one.
//!
//! Anything new the parser starts recording in a thread-local must be added to
//! `ParseEffects` too, or it becomes the next cache-state-dependent bug. A
//! deliberate non-entry: inline `module Foo { ... is export }` registrations
//! (`INLINE_MODULE_EXPORTS`) were measured to behave identically cold and warm,
//! because the importer's own uncached parse-time export scan registers them.
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
//! - The stored canonical source path matches the one being loaded. The file
//!   name is only a 64-bit hash of that path, so without this an (astronomically
//!   unlikely) hash collision would serve another module's AST; the entry has to
//!   be able to name itself.
//! - The source file modification time matches the stored mtime
//! - The source content hash matches the stored hash
//! - The interpreter version matches the stored version stamp. The stamp embeds
//!   the running executable's mtime, so a rebuilt mutsu (with different parse /
//!   prelude-injection / AST-lowering logic) never reuses an older build's cache
//!   — the stored AST is post-transform, and those transforms live in the binary.
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

/// Magic bytes for the cache format. Bump the trailing byte whenever the
/// on-disk encoding changes so stale caches are cleanly rejected by the magic
/// check rather than mis-decoded. `MTS2` marks the bincode 2 (varint) encoding.
const CACHE_MAGIC: &[u8; 4] = b"MTS2";

/// The interpreter version stamp embedded in cache files.
/// Cache is invalidated whenever this changes.
/// Includes the crate version, a cache format version that should be bumped
/// whenever the AST enum layout changes (adding/removing/reordering variants),
/// and the running executable's modification time.
///
/// The executable mtime is essential: the cache stores the AST *after* compile-
/// time transforms (the NativeCall `Pointer` / `Rational` prelude injection,
/// roast-directive preprocessing). Those transforms live in
/// the binary, not the source, so a rebuilt mutsu that changes any of them must
/// not reuse a cache produced by an older build. The crate version is a fixed
/// "0.1.0" across every dev build and CACHE_FORMAT_VERSION only tracks enum
/// layout, so neither catches logic changes — which is why a stale post-injection
/// AST could survive the prelude-injection fix and resurface as an undeclared
/// `Pointer`. Stamping the exe mtime invalidates the cache on every rebuild,
/// removing the need to manually `rm` the cache after parser/compiler changes.
fn interpreter_version() -> String {
    // Bump CACHE_FORMAT_VERSION when Stmt/Expr/Value enum variants change,
    // or when `CacheMetadata` / `ParseEffects` gain or lose a field.
    // 9: `Value` gained `BufStorage` (ADR-0015 P2), which also shifted the
    // serialized `SerValue` discriminants after `Array`.
    const CACHE_FORMAT_VERSION: u32 = 9;
    let exe_stamp = std::env::current_exe()
        .and_then(fs::metadata)
        .and_then(|m| m.modified())
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    format!(
        "{}+cf{}+exe{}",
        env!("CARGO_PKG_VERSION"),
        CACHE_FORMAT_VERSION,
        exe_stamp
    )
}

/// Whether the cache is on unless a caller turns it off.
///
/// `MUTSU_PRECOMP=0` disables it process-wide, which `--no-precomp` could not do:
/// that flag only reaches interpreters built by `main.rs`, so a test harness, a
/// CI step, or an embedding host had no way to exercise the no-cache path.
/// Any other value (or an unset variable) leaves the cache enabled.
pub(crate) fn enabled_by_default() -> bool {
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::env::var("MUTSU_PRECOMP")
            .map(|v| v != "0")
            .unwrap_or(true)
    }
    #[cfg(target_arch = "wasm32")]
    {
        true
    }
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

/// The thread-local parser state a module's parse leaves behind, which a cache
/// hit would otherwise skip. See the module docs for why this exists and what
/// belongs in it.
#[derive(Debug, Default, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct ParseEffects {
    /// The language revision the parser was left in — the module's own `use vX`,
    /// or the 6.d default. Replayed so the module's mainline runs under its own
    /// revision, exactly as it does on a cache miss.
    pub(crate) language_version: String,
    /// Warnings the parse emitted, so a warm run reports them like a cold one.
    pub(crate) warnings: Vec<String>,
}

/// A cached compilation unit: the AST plus the parse effects to replay.
pub(crate) struct CachedUnit {
    pub(crate) stmts: Vec<Stmt>,
    pub(crate) effects: ParseEffects,
}

/// Metadata stored alongside the cached AST.
#[derive(serde::Serialize, serde::Deserialize)]
struct CacheMetadata {
    /// Canonical path of the source this entry was built from. The cache file
    /// name is only a hash of it, so storing it lets the entry be verified
    /// rather than merely assumed to belong to the file being loaded.
    source_path: String,
    /// Source file modification time as nanoseconds since UNIX epoch.
    mtime_nanos: u128,
    /// Hash of source content at cache write time.
    source_hash: Option<u64>,
    /// Interpreter version at the time of caching.
    version: String,
    /// Parser side effects to replay on a hit.
    effects: ParseEffects,
}

/// Scratch path a cache entry is written to before being renamed into place.
///
/// The name must be unique per *writer*, not per entry. Two mutsu processes
/// loading the same module concurrently — `prove -j`, or a `Test::Util` parent
/// and the child it spawns — would otherwise interleave their non-atomic
/// `fs::write`s into one shared `<hash>.tmp` and rename the mixture into place.
/// The rename is atomic; the write into the shared buffer is not, so sharing the
/// buffer defeats the point of renaming.
fn temp_cache_path(cache_file: &Path) -> PathBuf {
    cache_file.with_extension(format!("{}.tmp", std::process::id()))
}

/// Upper bound on any single allocation a cache decode may request.
///
/// Decoding is fallible but *allocation* is not: `bincode` reads a length prefix
/// and asks for that much memory before it can report a mismatch, so a corrupt
/// entry made the allocator abort the whole process (SIGABRT) where `.ok()?`
/// looks like it would yield a clean cache miss. A limit turns that into a
/// `DecodeError`, i.e. a cache miss and a reparse.
///
/// 256 MiB is far above any real entry (the largest module ASTs are a few MiB)
/// and far below "the machine's memory", which is the only bound the encoding
/// itself imposes.
const MAX_DECODE_ALLOC: usize = 256 * 1024 * 1024;

fn decode_config() -> impl bincode::config::Config {
    bincode::config::standard().with_limit::<MAX_DECODE_ALLOC>()
}

/// Try to load a cached compilation unit for the given source file.
///
/// Returns `Some(unit)` if a valid cache entry exists, `None` otherwise.
pub(crate) fn load_cached_unit(source_path: &Path) -> Option<CachedUnit> {
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

    let (meta, _): (CacheMetadata, usize) =
        bincode::serde::decode_from_slice(&rest[..meta_len], decode_config()).ok()?;
    let ast_data = &rest[meta_len..];

    // Validate that the entry actually describes this file. The cache file name
    // is a 64-bit hash of the path, so a collision would otherwise hand back
    // another module's AST.
    if meta.source_path != canonical.to_string_lossy() {
        return None;
    }

    // Validate version
    if meta.version != interpreter_version() {
        // Version mismatch — remove stale cache
        let _ = fs::remove_file(&cache_file);
        return None;
    }

    // Validate mtime
    let source_mtime = source_mtime_nanos(source_path)?;
    if meta.mtime_nanos != source_mtime {
        // Source changed — remove stale cache
        let _ = fs::remove_file(&cache_file);
        return None;
    }
    // Validate source content hash. Missing hash indicates an old cache format;
    // drop it to avoid stale cache hits on coarse mtime filesystems.
    let Some(expected_hash) = meta.source_hash else {
        let _ = fs::remove_file(&cache_file);
        return None;
    };
    let current_hash = source_content_hash(source_path)?;
    if expected_hash != current_hash {
        let _ = fs::remove_file(&cache_file);
        return None;
    }

    bincode::serde::decode_from_slice(ast_data, decode_config())
        .ok()
        .map(|(stmts, _)| CachedUnit {
            stmts,
            effects: meta.effects,
        })
}

/// Save a parsed compilation unit — the AST *and* the parse effects to replay —
/// to the cache for the given source file.
///
/// Errors are silently ignored (cache is best-effort).
pub(crate) fn save_cached_unit(source_path: &Path, stmts: &[Stmt], effects: &ParseEffects) {
    let Some(canonical) = source_path.canonicalize().ok() else {
        return;
    };
    let Some(dir) = cache_dir() else {
        return;
    };
    let Some(source_mtime) = source_mtime_nanos(source_path) else {
        return;
    };
    prune_cache_once(&dir);

    let hash = path_hash(&canonical);
    let cache_file = dir.join(format!("{}.bin", hash));

    let meta = CacheMetadata {
        source_path: canonical.to_string_lossy().into_owned(),
        mtime_nanos: source_mtime,
        source_hash: source_content_hash(source_path),
        version: interpreter_version(),
        effects: effects.clone(),
    };

    let Ok(meta_bytes) = bincode::serde::encode_to_vec(&meta, bincode::config::standard()) else {
        return;
    };
    let Ok(ast_bytes) = bincode::serde::encode_to_vec(stmts, bincode::config::standard()) else {
        return;
    };

    let meta_len = meta_bytes.len() as u32;
    let mut data = Vec::with_capacity(4 + 4 + meta_bytes.len() + ast_bytes.len());
    data.extend_from_slice(CACHE_MAGIC);
    data.extend_from_slice(&meta_len.to_le_bytes());
    data.extend_from_slice(&meta_bytes);
    data.extend_from_slice(&ast_bytes);

    // Write atomically via temp file + rename.
    let tmp_file = temp_cache_path(&cache_file);
    if fs::write(&tmp_file, &data).is_ok() {
        if fs::rename(&tmp_file, &cache_file).is_err() {
            let _ = fs::remove_file(&tmp_file);
        }
    } else {
        let _ = fs::remove_file(&tmp_file);
    }
}

/// Clear all cached precompilation files.
#[allow(dead_code)]
pub(crate) fn clear_cache() {
    if let Some(dir) = cache_dir() {
        let _ = fs::remove_dir_all(&dir);
    }
}

/// Upper bound on cache entries before the oldest are evicted.
const MAX_CACHE_ENTRIES: usize = 4096;

/// Evict the oldest entries when the cache grows past `MAX_CACHE_ENTRIES`.
///
/// Nothing ever removed an entry whose source path stopped being loaded, so the
/// directory grew without bound — one real checkout had accumulated 12,355 files
/// across renamed/deleted modules and abandoned worktrees. Entries are cheap to
/// rebuild (that is the whole point of a cache), so evicting by file mtime —
/// which a regenerated entry refreshes, making this an approximate LRU — is
/// enough.
///
/// The same sweep also removes abandoned scratch files. `save_cached_unit`
/// cleans up its own on any failure it can observe, but a process killed between
/// the write and the rename leaves one behind, and since the name is now
/// per-pid (see `temp_cache_path`) it will never be reused. They are only
/// dropped once they are old enough that no live writer could still own them.
///
/// Runs at most once per process, and only from the save path, so a warm run
/// that never writes never pays for the scan.
fn prune_cache_once(dir: &Path) {
    static PRUNED: std::sync::OnceLock<()> = std::sync::OnceLock::new();
    if PRUNED.set(()).is_err() {
        return;
    }
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    const STALE_TEMP_AGE: std::time::Duration = std::time::Duration::from_secs(60 * 60);
    let mut files: Vec<(SystemTime, PathBuf)> = entries
        .flatten()
        .filter(|e| {
            let path = e.path();
            if path.extension().is_some_and(|ext| ext == "tmp") {
                let abandoned = e
                    .metadata()
                    .and_then(|m| m.modified())
                    .ok()
                    .and_then(|t| t.elapsed().ok())
                    .is_some_and(|age| age > STALE_TEMP_AGE);
                if abandoned {
                    let _ = fs::remove_file(&path);
                }
                return false;
            }
            path.extension().is_some_and(|ext| ext == "bin")
        })
        .filter_map(|e| {
            let modified = e.metadata().ok()?.modified().ok()?;
            Some((modified, e.path()))
        })
        .collect();
    if files.len() <= MAX_CACHE_ENTRIES {
        return;
    }
    // Oldest first, then drop everything past half the cap so the next prune is
    // not immediately due again.
    files.sort_by_key(|(modified, _)| *modified);
    let keep = MAX_CACHE_ENTRIES / 2;
    for (_, path) in files.iter().take(files.len() - keep) {
        let _ = fs::remove_file(path);
    }
}

/// Get the modification time of a file as seconds since UNIX epoch.
fn source_mtime_nanos(path: &Path) -> Option<u128> {
    let metadata = fs::metadata(path).ok()?;
    let modified = metadata.modified().ok()?;
    let duration = modified.duration_since(SystemTime::UNIX_EPOCH).ok()?;
    Some(duration.as_nanos())
}

fn source_content_hash(path: &Path) -> Option<u64> {
    let bytes = fs::read(path).ok()?;
    let mut hasher = DefaultHasher::new();
    bytes.hash(&mut hasher);
    Some(hasher.finish())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Stmt};
    use crate::value::Value;
    use crate::value::ValueView;
    use std::io::Write;

    #[test]
    fn roundtrip_simple_ast() {
        let stmts = vec![
            Stmt::Say(vec![Expr::Literal(Value::int(42))]),
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
        save_cached_unit(&source, &stmts, &ParseEffects::default());
        let loaded = load_cached_unit(&source);
        assert!(loaded.is_some(), "cache should return Some");
        let loaded = loaded.unwrap().stmts;
        assert_eq!(loaded.len(), 2);

        // Verify round-trip preserves structure
        assert!(matches!(&loaded[0], Stmt::Say(args) if args.len() == 1));
        assert!(matches!(
            &loaded[1],
            Stmt::Expr(Expr::Literal(lit)) if matches!(lit.view(), ValueView::Str(_))
        ));

        // Clean up
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn cache_invalidated_on_mtime_change() {
        let stmts = vec![Stmt::Say(vec![Expr::Literal(Value::int(1))])];

        let dir = tempdir("mtime");
        let source = dir.join("test2.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 1;").unwrap();
        }

        save_cached_unit(&source, &stmts, &ParseEffects::default());
        assert!(load_cached_unit(&source).is_some());

        // Touch the file (update mtime)
        std::thread::sleep(std::time::Duration::from_secs(2));
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 2;").unwrap();
        }

        // Cache should now be invalid
        assert!(load_cached_unit(&source).is_none());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn version_stamp_includes_exe_mtime() {
        // The version stamp must embed the running executable's mtime so a
        // rebuilt binary invalidates caches that hold post-transform ASTs.
        // The test runner is a real on-disk binary, so the stamp must resolve
        // to a non-zero value (a "+exe0" fallback would silently disable the
        // protection).
        let v = interpreter_version();
        assert!(
            v.contains("+exe"),
            "version stamp must carry exe component: {v}"
        );
        assert!(
            !v.ends_with("+exe0"),
            "exe mtime should resolve to a real timestamp, got: {v}"
        );
    }

    #[test]
    fn cache_invalidated_on_version_mismatch() {
        // A cache whose stored version no longer matches the current
        // interpreter_version() (as happens when the binary is rebuilt with
        // different parse/injection logic) must be rejected even when the
        // source file is byte-for-byte unchanged.
        let stmts = vec![Stmt::Say(vec![Expr::Literal(Value::int(7))])];

        let dir = tempdir("version");
        let source = dir.join("test3.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 7;").unwrap();
        }

        // Write a cache entry by hand with a stale version stamp.
        let canonical = source.canonicalize().unwrap();
        let cdir = cache_dir().unwrap();
        let cache_file = cdir.join(format!("{}.bin", path_hash(&canonical)));
        let meta = CacheMetadata {
            source_path: canonical.to_string_lossy().into_owned(),
            mtime_nanos: source_mtime_nanos(&source).unwrap(),
            source_hash: source_content_hash(&source),
            version: "0.0.0+cf0+exe0".to_string(),
            effects: ParseEffects::default(),
        };
        let meta_bytes = bincode::serde::encode_to_vec(&meta, bincode::config::standard()).unwrap();
        let ast_bytes = bincode::serde::encode_to_vec(&stmts, bincode::config::standard()).unwrap();
        let mut data = Vec::new();
        data.extend_from_slice(CACHE_MAGIC);
        data.extend_from_slice(&(meta_bytes.len() as u32).to_le_bytes());
        data.extend_from_slice(&meta_bytes);
        data.extend_from_slice(&ast_bytes);
        fs::write(&cache_file, &data).unwrap();

        // Stale version → cache must be rejected (and removed).
        assert!(load_cached_unit(&source).is_none());
        assert!(!cache_file.exists(), "stale cache file should be removed");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn parse_effects_survive_the_round_trip() {
        // The whole point of the entry: a hit must be able to replay what the
        // skipped parse would have left in the parser's thread-locals.
        let stmts = vec![Stmt::Say(vec![Expr::Literal(Value::int(1))])];
        let effects = ParseEffects {
            language_version: "6.e".to_string(),
            warnings: vec!["Potential difficulties:\n    Duplicate 'is export' trait".to_string()],
        };

        let dir = tempdir("effects");
        let source = dir.join("effects.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "use v6.e.PREVIEW; say 1;").unwrap();
        }

        save_cached_unit(&source, &stmts, &effects);
        let loaded = load_cached_unit(&source).expect("cache should return Some");
        assert_eq!(loaded.effects, effects);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn entry_rejected_when_it_names_a_different_source() {
        // The cache file name is only a 64-bit hash of the path, so an entry has
        // to be able to prove it belongs to the file being loaded rather than
        // being trusted because it sits at the expected name.
        let stmts = vec![Stmt::Say(vec![Expr::Literal(Value::int(3))])];

        let dir = tempdir("identity");
        let source = dir.join("identity.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 3;").unwrap();
        }

        save_cached_unit(&source, &stmts, &ParseEffects::default());
        assert!(load_cached_unit(&source).is_some());

        // Rewrite the entry claiming a different source path, as a path-hash
        // collision would produce.
        let canonical = source.canonicalize().unwrap();
        let cdir = cache_dir().unwrap();
        let cache_file = cdir.join(format!("{}.bin", path_hash(&canonical)));
        let meta = CacheMetadata {
            source_path: "/somewhere/else/Other.rakumod".to_string(),
            mtime_nanos: source_mtime_nanos(&source).unwrap(),
            source_hash: source_content_hash(&source),
            version: interpreter_version(),
            effects: ParseEffects::default(),
        };
        let meta_bytes = bincode::serde::encode_to_vec(&meta, bincode::config::standard()).unwrap();
        let ast_bytes = bincode::serde::encode_to_vec(&stmts, bincode::config::standard()).unwrap();
        let mut data = Vec::new();
        data.extend_from_slice(CACHE_MAGIC);
        data.extend_from_slice(&(meta_bytes.len() as u32).to_le_bytes());
        data.extend_from_slice(&meta_bytes);
        data.extend_from_slice(&ast_bytes);
        fs::write(&cache_file, &data).unwrap();

        assert!(
            load_cached_unit(&source).is_none(),
            "an entry naming another source must not be served"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_corrupt_entry_is_a_cache_miss_not_an_abort() {
        // Two mutsu processes writing the same entry used to share one
        // `<hash>.tmp` and rename a mixture of both encodings into place. The
        // reader then took a length prefix out of the middle of the other
        // process's bytes and handed it to bincode, which *allocates before it
        // can fail* — so the whole process died with
        // "memory allocation of 1784363464925575909 bytes failed", not the
        // clean `None` that `.ok()?` reads as. The temp file is unique per
        // process now; this pins the second half of the fix, that an entry which
        // is corrupt for any other reason still only costs a reparse.
        let stmts = vec![Stmt::Say(vec![Expr::Literal(Value::int(11))])];

        let dir = tempdir("corrupt");
        let source = dir.join("corrupt.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 11;").unwrap();
        }

        save_cached_unit(&source, &stmts, &ParseEffects::default());
        assert!(
            load_cached_unit(&source).is_some(),
            "control: entry is valid"
        );

        let canonical = source.canonicalize().unwrap();
        let cache_file = cache_dir()
            .unwrap()
            .join(format!("{}.bin", path_hash(&canonical)));

        // Keep the magic bytes so the cheap header check passes, then hand the
        // metadata decoder a `String` whose bincode varint length prefix claims
        // ~1.7 exabytes -- the exact shape of the observed abort. In bincode 2's
        // varint encoding a leading 253 means "a u64 length follows", so this is
        // what a length prefix read out of the middle of unrelated bytes looks
        // like. It must be a *plausible* prefix: a byte that is not a valid
        // marker at all (0xFF) errors out before any allocation is attempted, so
        // it would not exercise the limit.
        let mut garbage = vec![253u8];
        garbage.extend_from_slice(&1_784_363_464_925_575_909u64.to_le_bytes());
        garbage.resize(64, 0);
        let mut data = Vec::new();
        data.extend_from_slice(CACHE_MAGIC);
        data.extend_from_slice(&(garbage.len() as u32).to_le_bytes());
        data.extend_from_slice(&garbage);
        data.extend_from_slice(&garbage);
        fs::write(&cache_file, &data).unwrap();

        assert!(
            load_cached_unit(&source).is_none(),
            "a corrupt entry must read as a cache miss"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn concurrent_writers_do_not_share_a_temp_file() {
        // The bug in one line: `cache_file.with_extension("tmp")` is the same
        // path in every process. Whatever the temp name is now, it must carry
        // something process-unique.
        let dir = tempdir("tmpname");
        let source = dir.join("tmpname.rakumod");
        {
            let mut f = fs::File::create(&source).unwrap();
            writeln!(f, "say 13;").unwrap();
        }
        let canonical = source.canonicalize().unwrap();
        let cache_file = cache_dir()
            .unwrap()
            .join(format!("{}.bin", path_hash(&canonical)));

        let tmp = temp_cache_path(&cache_file);
        assert_ne!(
            tmp,
            cache_file.with_extension("tmp"),
            "the temp name must not be shared across processes"
        );
        assert!(
            tmp.to_string_lossy()
                .contains(&std::process::id().to_string()),
            "the temp name must be process-unique, got {tmp:?}"
        );

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
