//! The bundled batteries' module search paths (`modules/<Dist>/lib` shipped
//! alongside the binary, BATTERIES.md §3/§6): where the bundle is, and the
//! process-wide scan of its distributions.
//!
//! Plain filesystem logic below the runtime, so the parser can expand its
//! search paths with the bundle only when a parse resolves a `use`, rather
//! than every run handing it a scanned list (#19197).

use std::sync::Arc;

/// The process-wide memo behind [`shared`],
/// keyed by the `MUTSU_BUNDLE_DIR` value the last scan was made under so a
/// process that changes it still re-scans.
#[allow(clippy::type_complexity)]
fn memo() -> &'static std::sync::RwLock<Option<(Option<String>, Arc<Vec<String>>)>> {
    static MEMO: std::sync::RwLock<Option<(Option<String>, Arc<Vec<String>>)>> =
        std::sync::RwLock::new(None);
    &MEMO
}

/// [`resolve`], scanned at most once per process
/// (per `MUTSU_BUNDLE_DIR` value) and handed out as a shared `Arc`.
///
/// The scan is a `read_dir` of the bundle base plus a `join` + `is_dir`
/// stat for every distribution in it — ~40 entries, ~130 KB of
/// instructions. `Interpreter::new` used to run it unconditionally, and a
/// grammar-with-actions parse builds one scratch interpreter per
/// subrule-with-arguments call and per embedded code block: 3,109 of them
/// on a 60-row `benchmarks/bench-yaml-parse.raku` document, i.e. 3,109 full
/// directory scans (124,360 `DirEntry::path`s, 133,687 `is_dir` stats) of a
/// directory whose contents cannot change under us, for 6.7% of the whole
/// program's instructions (#7576 round 12).
///
/// The answer depends only on `MUTSU_BUNDLE_DIR` and the running
/// executable's location, so it is memoized on the former (the latter
/// cannot change within a process). A bundle directory whose *contents*
/// change mid-process is not picked up — adding a module search path at
/// runtime is what `-I` / `MUTSULIB` / `use lib` are for.
pub(crate) fn shared() -> Arc<Vec<String>> {
    let bundle_dir = std::env::var("MUTSU_BUNDLE_DIR").ok();
    if let Ok(memo) = memo().read()
        && let Some((key, paths)) = memo.as_ref()
        && *key == bundle_dir
    {
        return Arc::clone(paths);
    }
    let paths = Arc::new(resolve());
    if let Ok(mut memo) = memo().write() {
        *memo = Some((bundle_dir, Arc::clone(&paths)));
    }
    paths
}

pub(crate) fn resolve() -> Vec<String> {
    let Some(base) = bundle_base() else {
        return Vec::new();
    };
    let Ok(entries) = std::fs::read_dir(&base) else {
        return Vec::new();
    };
    let mut paths: Vec<String> = Vec::new();
    for entry in entries.flatten() {
        let lib = entry.path().join("lib");
        if lib.is_dir() {
            paths.push(lib.display().to_string());
        }
    }
    paths.sort();
    paths
}

/// The directory holding the bundled distributions (`MUTSU_BUNDLE_DIR`,
/// or the first of the exe-relative candidates that exists), without
/// scanning it.
// Cost: O(1): an environment read, `current_exe` and a few `stat`s.
pub(crate) fn bundle_base() -> Option<std::path::PathBuf> {
    use std::path::PathBuf;
    let base: Option<PathBuf> = if let Ok(dir) = std::env::var("MUTSU_BUNDLE_DIR") {
        Some(PathBuf::from(dir))
    } else {
        std::env::current_exe().ok().and_then(|exe| {
            let dir = exe.parent()?.to_path_buf();
            let mut candidates = vec![
                dir.join("..").join("share").join("mutsu").join("modules"),
                dir.join("..").join("..").join("modules"),
                dir.join("modules"),
            ];
            // A cargo test binary lives one level deeper, in
            // `target/<profile>/deps/`, so the dev-build candidate above
            // lands on `target/modules` and finds nothing — every `use`
            // of a bundled battery inside a `#[test]` then failed with
            // "Could not find <Module> in: (module repositories)". Only
            // taken when the binary really sits in a `deps` directory, so
            // an installed layout can't reach a stray `modules/` three
            // levels up.
            if dir.file_name().is_some_and(|n| n == "deps") {
                candidates.push(dir.join("..").join("..").join("..").join("modules"));
            }
            // Cargo 1.100's build-dir layout puts a test binary in
            // `target/<profile>/build/<pkg>/<hash>/out/`, which is neither
            // `deps` nor near `modules/`. Taken only when the path really
            // is `<root>/target/<profile>/build/..`, so an installed
            // layout can't reach a stray `modules/` through it.
            if let Some(build) = dir
                .ancestors()
                .find(|a| a.file_name().is_some_and(|n| n == "build"))
                && let Some(target) = build.parent().and_then(|p| p.parent())
                && target.file_name().is_some_and(|n| n == "target")
                && let Some(root) = target.parent()
            {
                candidates.push(root.join("modules"));
            }
            candidates.into_iter().find(|c| c.is_dir())
        })
    };
    base.filter(|b| b.is_dir())
}
