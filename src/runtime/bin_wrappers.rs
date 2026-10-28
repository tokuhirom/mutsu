//! The launcher scripts an installation puts in `<prefix>/bin`, as Rakudo's
//! `CompUnit::Repository::Installation` does: `bin/NAME` is what a user (or a
//! test) puts on `PATH` and runs, `bin/NAME.raku` is the program it starts (a bare statement, not a `sub MAIN`: the
//! script it loads brings its own `MAIN`), and
//! the distribution's own script stays under its content id (`files{"bin/NAME"}`),
//! where `CompUnit::RepositoryRegistry.run-script` finds it.

use std::path::Path;

/// Write `bin/NAME` and `bin/NAME.raku` for the script `name`.
///
/// The shell launcher runs this very executable with the installation put in
/// front of the repository chain, so the script finds its distribution's modules.
// Cost: O(|name|) plus two file writes.
pub(crate) fn write_bin_wrapper(prefix: &Path, name: &str) {
    let bin_dir = prefix.join("bin");
    let exe = std::env::current_exe()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| "mutsu".to_string());
    let launcher = format!(
        "#!/usr/bin/env sh\n\nDIR=$(cd \"$(dirname -- \"$0\")\" && pwd -P)\n\nexec \"{exe}\" -I \"inst#${{DIR%/bin}}\" \"${{DIR}}/{name}.raku\" \"$@\"\n"
    );
    let program =
        format!("CompUnit::RepositoryRegistry.run-script(\"{name}\");\n");
    let launcher_path = bin_dir.join(name);
    if std::fs::write(&launcher_path, launcher).is_ok() {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(&launcher_path, std::fs::Permissions::from_mode(0o755)).ok();
        }
    }
    std::fs::write(bin_dir.join(format!("{name}.raku")), program).ok();
}

/// Remove what [`write_bin_wrapper`] wrote for `name`.
// Cost: O(1).
pub(crate) fn remove_bin_wrapper(prefix: &Path, name: &str) {
    let bin_dir = prefix.join("bin");
    std::fs::remove_file(bin_dir.join(name)).ok();
    std::fs::remove_file(bin_dir.join(format!("{name}.raku"))).ok();
}
