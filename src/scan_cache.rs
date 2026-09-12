//! On-disk cache for the parser's module *export scan*.
//!
//! A `use Foo;` makes mutsu read `Foo`'s source twice: the parser scans it to
//! learn what it exports (so the importing unit can parse calls to those
//! exports), and the runtime loads it to execute it. The runtime half has had a
//! precompilation cache for a long time ([`crate::precomp`]); the parser half
//! had none, so every process re-parsed every `use`d module's source from
//! scratch. That scan was measured at 209 M instructions — 63% of a warm
//! `use YAMLish; say "ok"` run (GH-8095).
//!
//! This module gives the scan the same treatment, with one extra validation
//! obligation the precompilation cache does not have.
//!
//! ## Why the module's own source hash is not a sufficient key
//!
//! A scan result deliberately carries **transitive** names: the type names,
//! enum values and constant terms that reached the module from the modules *it*
//! `use`s, because an importer can see those too. So the result is a function
//! of the dependency sources as well, and validating only the module's own
//! source would serve a stale answer after a dependency is edited.
//!
//! Each entry therefore records every module the scan resolved, as a
//! [`ScanDep`]: the module name as written, the file it resolved to (or `None`
//! when it resolved to nothing the parser could scan), and that file's stamp.
//! The list is the *transitive* closure — a child scan's deps are merged into
//! its parent's — so validating one entry validates the whole subtree without
//! loading the children's entries.
//!
//! Validation re-resolves each dependency's module name through the parser's
//! current search path and requires it to land on the same file, which is what
//! makes the cache safe across a changed `-I` / `use lib`: a module that now
//! resolves elsewhere invalidates the entry rather than silently keeping the
//! old file's names.
//!
//! ## What is deliberately not persisted
//!
//! The parse warnings the scan raises. They are re-raised (and deduplicated by
//! `(file, message)`) when the runtime actually loads the module, which always
//! happens for a `use`, so a hit loses nothing the user sees. The in-process
//! scan memo has behaved this way for every `use` after the first since it was
//! added.

use std::path::{Path, PathBuf};

use crate::precomp;

/// Identity of a source file at scan time: both halves of the precompilation
/// cache's own validity test, so a dependency is judged exactly as strictly as
/// the unit it belongs to.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct FileStamp {
    pub(crate) mtime_nanos: u128,
    pub(crate) hash: u64,
}

impl FileStamp {
    /// Stamp a file whose content the caller already has in memory.
    pub(crate) fn of_source(path: &Path, source: &str) -> Option<Self> {
        Some(FileStamp {
            mtime_nanos: precomp::source_mtime_nanos(path)?,
            hash: precomp::content_hash(source.as_bytes()),
        })
    }

    /// Stamp a file by reading it.
    fn of_path(path: &Path) -> Option<Self> {
        Some(FileStamp {
            mtime_nanos: precomp::source_mtime_nanos(path)?,
            hash: precomp::source_content_hash(path)?,
        })
    }
}

/// One module the scan resolved, recorded so the entry can be invalidated when
/// that module changes — or when the same name now resolves to a different
/// file.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct ScanDep {
    /// Module name exactly as the `use`/`need` wrote it.
    pub(crate) module: String,
    /// The file it resolved to, or `None` when it resolved to no scannable
    /// file (a pragma, a native provider, an installed repository the parser
    /// cannot walk). Recorded either way: a name that resolves to nothing
    /// today and to a file tomorrow changes the scan's answer.
    pub(crate) path: Option<String>,
    /// Stamp of `path`, when there is one.
    pub(crate) stamp: Option<FileStamp>,
}

/// Metadata stored ahead of the serialized scan payload.
#[derive(serde::Serialize, serde::Deserialize)]
struct ScanMetadata {
    /// Canonical path of the module this entry describes. The cache file name
    /// is only a hash of it, so storing it lets the entry name itself rather
    /// than be assumed to belong to the file being loaded.
    source_path: String,
    /// Stamp of that module's own source.
    stamp: FileStamp,
    /// Interpreter version stamp (embeds the executable's mtime, so a rebuilt
    /// mutsu with different scan logic never reuses an older build's entries).
    version: String,
    /// Every module the scan resolved, transitively. See the module docs.
    deps: Vec<ScanDep>,
}

/// Magic bytes for the scan-cache format. `MSC1` marks version 1; bump the
/// trailing byte whenever the framing changes so stale files are rejected by
/// the magic check rather than mis-decoded.
const CACHE_MAGIC: &[u8; 4] = b"MSC1";

fn cache_dir() -> std::io::Result<PathBuf> {
    precomp::sibling_cache_dir("modscan")
}

/// A validated cache entry.
pub(crate) struct LoadedScan<T> {
    pub(crate) payload: T,
    pub(crate) deps: Vec<ScanDep>,
    pub(crate) stamp: FileStamp,
}

/// Whether a dependency record still describes the world the scan saw.
fn dep_is_valid(dep: &ScanDep, resolve: &dyn Fn(&str) -> Option<String>) -> bool {
    let resolved = resolve(&dep.module);
    if resolved != dep.path {
        return false;
    }
    match (&dep.path, &dep.stamp) {
        (None, _) => true,
        (Some(path), Some(stamp)) => FileStamp::of_path(Path::new(path)).as_ref() == Some(stamp),
        // A resolved path with no stamp means the scan could not read the file;
        // never treat that as a reusable observation.
        (Some(_), None) => false,
    }
}

/// Load the cached scan for `source_path`, if one is valid.
///
/// `resolve` maps a module name to the file the parser would resolve it to
/// right now; it is how a changed search path invalidates the entry.
pub(crate) fn load<T: serde::de::DeserializeOwned>(
    source_path: &Path,
    resolve: &dyn Fn(&str) -> Option<String>,
) -> Option<LoadedScan<T>> {
    if !precomp::enabled_for_process() {
        return None;
    }
    let canonical = source_path.canonicalize().ok()?;
    let cache_file = cache_dir().ok()?.join(format!(
        "{}.bin",
        precomp::path_hash_hex(&canonical.to_string_lossy())
    ));
    let data = std::fs::read(&cache_file).ok()?;
    if data.len() < 8 || &data[0..4] != CACHE_MAGIC {
        return None;
    }
    let meta_len = u32::from_le_bytes([data[4], data[5], data[6], data[7]]) as usize;
    let rest = &data[8..];
    if rest.len() < meta_len {
        return None;
    }
    let (meta, _): (ScanMetadata, usize) =
        bincode::serde::decode_from_slice(&rest[..meta_len], precomp::decode_config()).ok()?;

    if meta.source_path != canonical.to_string_lossy() {
        return None;
    }
    if meta.version != precomp::interpreter_version() {
        let _ = std::fs::remove_file(&cache_file);
        return None;
    }
    if FileStamp::of_path(source_path).as_ref() != Some(&meta.stamp) {
        let _ = std::fs::remove_file(&cache_file);
        return None;
    }
    // A dependency change does not invalidate this file's *own* identity, so
    // the entry is merely unusable now, not garbage — leave it on disk; the
    // re-scan that follows overwrites it.
    if !meta.deps.iter().all(|dep| dep_is_valid(dep, resolve)) {
        return None;
    }

    let (payload, _): (T, usize) =
        bincode::serde::decode_from_slice(&rest[meta_len..], precomp::decode_config()).ok()?;
    Some(LoadedScan {
        payload,
        deps: meta.deps,
        stamp: meta.stamp,
    })
}

/// Store a scan result for `source_path`. Best effort: every failure is a
/// silent no-op, since the only consequence is a re-scan next time.
pub(crate) fn save<T: serde::Serialize>(
    source_path: &Path,
    payload: &T,
    deps: &[ScanDep],
    stamp: &FileStamp,
) {
    if !precomp::enabled_for_process() {
        return;
    }
    let Ok(canonical) = source_path.canonicalize() else {
        return;
    };
    let Ok(dir) = cache_dir() else {
        return;
    };
    static PRUNED: std::sync::OnceLock<()> = std::sync::OnceLock::new();
    precomp::prune_cache_once(&dir, &PRUNED);
    let meta = ScanMetadata {
        source_path: canonical.to_string_lossy().into_owned(),
        stamp: stamp.clone(),
        version: precomp::interpreter_version(),
        deps: deps.to_vec(),
    };
    let Ok(meta_bytes) = bincode::serde::encode_to_vec(&meta, bincode::config::standard()) else {
        return;
    };
    let Ok(payload_bytes) = bincode::serde::encode_to_vec(payload, bincode::config::standard())
    else {
        return;
    };
    let mut data = Vec::with_capacity(8 + meta_bytes.len() + payload_bytes.len());
    data.extend_from_slice(CACHE_MAGIC);
    data.extend_from_slice(&(meta_bytes.len() as u32).to_le_bytes());
    data.extend_from_slice(&meta_bytes);
    data.extend_from_slice(&payload_bytes);

    let cache_file = dir.join(format!(
        "{}.bin",
        precomp::path_hash_hex(&canonical.to_string_lossy())
    ));
    // Same atomicity rule as the precompilation cache: the scratch name is
    // per-writer, because two `prove -j` processes scanning the same module
    // would otherwise interleave their non-atomic writes into one buffer and
    // rename the mixture into place.
    let tmp_file = cache_file.with_extension(format!("{}.tmp", std::process::id()));
    if std::fs::write(&tmp_file, &data).is_ok() {
        if std::fs::rename(&tmp_file, &cache_file).is_err() {
            let _ = std::fs::remove_file(&tmp_file);
        }
    } else {
        let _ = std::fs::remove_file(&tmp_file);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Debug, serde::Serialize, serde::Deserialize)]
    struct Payload {
        names: Vec<String>,
    }

    fn tempdir(tag: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "mutsu-scan-cache-{}-{}-{:?}",
            std::process::id(),
            tag,
            std::thread::current().id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    /// Point both caches at a scratch directory so the test never touches the
    /// developer's real `~/.cache/mutsu`.
    fn with_scratch_cache<R>(tag: &str, f: impl FnOnce(&Path) -> R) -> R {
        let dir = tempdir(tag);
        precomp::set_test_cache_dir(Some(dir.join("precomp")));
        let out = f(&dir);
        precomp::set_test_cache_dir(None);
        let _ = std::fs::remove_dir_all(&dir);
        out
    }

    fn write(path: &Path, content: &str) {
        std::fs::write(path, content).unwrap();
    }

    #[test]
    fn round_trips_a_payload() {
        with_scratch_cache("roundtrip", |dir| {
            let module = dir.join("Mod.rakumod");
            write(&module, "sub a is export {}\n");
            let stamp = FileStamp::of_source(&module, "sub a is export {}\n").unwrap();
            let payload = Payload {
                names: vec!["a".to_string()],
            };
            save(&module, &payload, &[], &stamp);
            let loaded: LoadedScan<Payload> = load(&module, &|_| None).expect("cache hit");
            assert_eq!(loaded.payload, payload);
        });
    }

    #[test]
    fn edited_source_misses() {
        with_scratch_cache("edited", |dir| {
            let module = dir.join("Mod.rakumod");
            write(&module, "sub a is export {}\n");
            let stamp = FileStamp::of_source(&module, "sub a is export {}\n").unwrap();
            save(
                &module,
                &Payload {
                    names: vec!["a".to_string()],
                },
                &[],
                &stamp,
            );
            write(&module, "sub b is export {}\n");
            let loaded: Option<LoadedScan<Payload>> = load(&module, &|_| None);
            assert!(loaded.is_none(), "an edited module must not hit");
        });
    }

    /// The invalidation obligation this cache has and the precompilation cache
    /// does not: a scan carries names that came from its dependencies, so
    /// editing a dependency must invalidate the importer's entry.
    #[test]
    fn edited_dependency_misses() {
        with_scratch_cache("dep", |dir| {
            let module = dir.join("Mod.rakumod");
            let dep = dir.join("Dep.rakumod");
            write(&module, "use Dep;\n");
            write(&dep, "class Dep::Thing {}\n");
            let stamp = FileStamp::of_source(&module, "use Dep;\n").unwrap();
            let deps = vec![ScanDep {
                module: "Dep".to_string(),
                path: Some(dep.to_string_lossy().into_owned()),
                stamp: FileStamp::of_path(&dep),
            }];
            let payload = Payload {
                names: vec!["Dep::Thing".to_string()],
            };
            save(&module, &payload, &deps, &stamp);
            let resolve = |name: &str| (name == "Dep").then(|| dep.to_string_lossy().into_owned());
            let hit: Option<LoadedScan<Payload>> = load(&module, &resolve);
            assert!(hit.is_some(), "an untouched dependency must still hit");

            write(&dep, "class Dep::Other {}\n");
            let miss: Option<LoadedScan<Payload>> = load(&module, &resolve);
            assert!(miss.is_none(), "an edited dependency must invalidate");
        });
    }

    /// A dependency that now resolves to a different file (a changed `-I`)
    /// invalidates too, even when neither file was edited.
    #[test]
    fn re_resolved_dependency_misses() {
        with_scratch_cache("resolve", |dir| {
            let module = dir.join("Mod.rakumod");
            let dep = dir.join("Dep.rakumod");
            let other = dir.join("Other.rakumod");
            write(&module, "use Dep;\n");
            write(&dep, "class Dep::Thing {}\n");
            write(&other, "class Other::Thing {}\n");
            let stamp = FileStamp::of_source(&module, "use Dep;\n").unwrap();
            let deps = vec![ScanDep {
                module: "Dep".to_string(),
                path: Some(dep.to_string_lossy().into_owned()),
                stamp: FileStamp::of_path(&dep),
            }];
            save(
                &module,
                &Payload {
                    names: vec!["Dep::Thing".to_string()],
                },
                &deps,
                &stamp,
            );
            let miss: Option<LoadedScan<Payload>> =
                load(&module, &|_| Some(other.to_string_lossy().into_owned()));
            assert!(miss.is_none(), "a re-resolved dependency must invalidate");
        });
    }

    /// A dependency that resolved to nothing is recorded too: it becoming
    /// resolvable changes the scan's answer.
    #[test]
    fn newly_resolvable_dependency_misses() {
        with_scratch_cache("unresolved", |dir| {
            let module = dir.join("Mod.rakumod");
            write(&module, "use Absent;\n");
            let stamp = FileStamp::of_source(&module, "use Absent;\n").unwrap();
            let deps = vec![ScanDep {
                module: "Absent".to_string(),
                path: None,
                stamp: None,
            }];
            save(&module, &Payload { names: vec![] }, &deps, &stamp);
            let hit: Option<LoadedScan<Payload>> = load(&module, &|_| None);
            assert!(hit.is_some(), "still-absent dependency must hit");
            let miss: Option<LoadedScan<Payload>> =
                load(&module, &|_| Some("/nowhere/Absent.rakumod".to_string()));
            assert!(
                miss.is_none(),
                "a now-resolvable dependency must invalidate"
            );
        });
    }
}
