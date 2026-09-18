//! Emitting the profile at process exit (ADR-0106 Slice 5).
//!
//! This is the whole output path, and it is deliberately thin: fold both
//! halves, build one [`document`], then render it as JSON to a
//! file and/or as text to stderr according to `--profile-report`. Nothing here
//! decides *what* a number means — that is the document's job — so adding a
//! renderer (HTML, a MoarVM-shaped export; ADR-0106 Slice 6) touches this file
//! only to call it.
//!
//! **stderr, never stdout.** The profiled program owns stdout; a report written
//! there would corrupt the output of the very program being profiled, and would
//! be invisible to anyone piping it.

use super::document;
use super::options;
use super::text;
use super::{counts, snapshot};

/// Fold every thread at process shutdown and emit the report.
pub(crate) fn flush_at_exit() {
    if !crate::vm::vm_poll::profiler_armed() {
        return;
    }
    let Some(options) = options::get() else {
        // Armed through the test hook rather than through the options (the JIT
        // site-ABI test in `vm_poll`), so there is no report to emit.
        return;
    };
    // Both snapshots drain their tables, so this runs exactly once.
    let profile = document::build(
        options,
        counts::take_counts(),
        snapshot::take_samples(),
        crate::alloc_stats::take_line_stats(),
    );
    if options.report.text() {
        eprint!("{}", text::render(&profile));
    }
    if let (true, Some(path)) = (options.report.json(), options.out.as_deref()) {
        match write_json(path, &profile) {
            // Rakudo's wording for the same event, on the same stream.
            Ok(()) => eprintln!("Writing profiler output to {}", path.display()),
            Err(err) => eprintln!(
                "[mutsu profiler] warning: cannot write {}: {err}",
                path.display()
            ),
        }
    }
}

fn write_json(path: &std::path::Path, profile: &document::Profile) -> std::io::Result<()> {
    // Pretty-printed: the document is read by people at least as often as by
    // tools, and `git diff` of two profiles is one of the uses ADR-0106 D7
    // names.
    let json = serde_json::to_string_pretty(profile)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))?;
    if let Some(parent) = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, json + "\n")
}
