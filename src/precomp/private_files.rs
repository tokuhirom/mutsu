//! The caches' files are private to the user who runs mutsu (docs/security.md,
//! "Caches are keyed and private"; #14516).
//!
//! Since ADR-11756 a precompilation entry holds bytecode the next load
//! executes, so an entry another user could plant or rewrite would run as the
//! user who loads the module. Keying (path, content hash, binary identity)
//! cannot stop that: a planted entry can carry the right keys. What stops it is
//! that nobody else can write the files:
//!
//! - every cache directory mutsu creates is `0700`; one it finds is accepted only
//!   if it is a real directory (not a symlink) owned by the current user, and is
//!   tightened to `0700` if it was created looser (older mutsu builds used the
//!   umask);
//! - an entry is written to a scratch file opened `O_CREAT|O_EXCL|O_NOFOLLOW`
//!   with mode `0600`, then renamed into place;
//! - an entry is read through `O_NOFOLLOW` and used only if it is a regular file
//!   owned by the current user that neither group nor others can write.
//!
//! A check that fails is a cache miss (or, for a directory, "no cache"), never an
//! error the program sees. Builds without a POSIX user model (wasm32, Miri, a
//! build without the `native` feature) keep the plain file operations.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// The POSIX implementation: modes, `O_NOFOLLOW`/`O_EXCL` and the owner check.
#[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]
mod imp {
    use std::fs::{self, DirBuilder, File, OpenOptions, Permissions};
    use std::io::{self, Read, Write};
    use std::os::unix::fs::{DirBuilderExt, MetadataExt, OpenOptionsExt, PermissionsExt};
    use std::path::Path;

    fn euid() -> u32 {
        // SAFETY: `geteuid` takes no arguments, has no preconditions and
        // cannot fail (POSIX).
        unsafe { libc::geteuid() }
    }

    fn denied(what: &str, path: &Path) -> io::Error {
        io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!("{what}: {}", path.display()),
        )
    }

    // Cost: O(d) system calls, d = missing path components.
    pub(super) fn ensure_private_dir(dir: &Path) -> io::Result<()> {
        DirBuilder::new().recursive(true).mode(0o700).create(dir)?;
        let meta = fs::symlink_metadata(dir)?;
        if !meta.file_type().is_dir() {
            return Err(denied("cache directory is not a directory", dir));
        }
        if meta.uid() != euid() {
            return Err(denied("cache directory is owned by another user", dir));
        }
        if meta.mode() & 0o077 != 0 {
            fs::set_permissions(dir, Permissions::from_mode(0o700))?;
        }
        Ok(())
    }

    // Cost: O(n), n = size of the file.
    pub(super) fn read_private(path: &Path) -> io::Result<Vec<u8>> {
        let mut file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
            .open(path)?;
        let meta = file.metadata()?;
        if !meta.file_type().is_file() {
            return Err(denied("cache entry is not a regular file", path));
        }
        if meta.uid() != euid() || meta.mode() & 0o022 != 0 {
            return Err(denied("cache entry is not private", path));
        }
        let mut data = Vec::with_capacity(meta.len() as usize);
        file.read_to_end(&mut data)?;
        Ok(data)
    }

    // Cost: O(n), n = data.len().
    pub(super) fn create_private(path: &Path, data: &[u8]) -> io::Result<()> {
        let mut file: File = OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
            .open(path)?;
        file.write_all(data)
    }
}

/// Builds without a POSIX user model: the plain file operations.
#[cfg(not(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native")))]
mod imp {
    use std::fs;
    use std::io;
    use std::path::Path;

    pub(super) fn ensure_private_dir(dir: &Path) -> io::Result<()> {
        fs::create_dir_all(dir)
    }

    pub(super) fn read_private(path: &Path) -> io::Result<Vec<u8>> {
        fs::read(path)
    }

    pub(super) fn create_private(path: &Path, data: &[u8]) -> io::Result<()> {
        fs::write(path, data)
    }
}

/// Create `dir` if needed and check that it is a private directory of the
/// current user, tightening its mode to `0700` if it is ours but looser.
///
/// Only call it for directories mutsu owns (`<cache>/mutsu` and below): the
/// user's own cache root (`$XDG_CACHE_HOME`, `~/.cache`) is created `0700` if
/// it is missing but is otherwise none of mutsu's business.
// Cost: O(d) system calls, d = missing path components.
pub(crate) fn ensure_private_dir(dir: &Path) -> io::Result<()> {
    imp::ensure_private_dir(dir)
}

/// Read a cache entry, or fail if it is not a private regular file of the
/// current user (a symlink, another user's file, or one others can write).
// Cost: O(n), n = size of the file.
pub(crate) fn read_private(path: &Path) -> io::Result<Vec<u8>> {
    imp::read_private(path)
}

/// Scratch path an entry is written to before it is renamed into place.
///
/// Unique per writer: per process, so two `prove -j` processes loading the
/// same module do not interleave their writes into one buffer, and per call,
/// so two threads of one process do not either (the scratch file is created
/// exclusively, and a second writer must not fail on the first one's name).
/// The `.tmp` extension is what `prune_cache_once` sweeps when a writer died
/// between the write and the rename.
// Cost: O(1).
pub(crate) fn scratch_path(entry: &Path) -> PathBuf {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let n = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    entry.with_extension(format!("{}-{n}.tmp", std::process::id()))
}

/// Write `data` as the cache entry `path`: to a fresh private scratch file
/// first, then renamed over `path`, so a reader sees the old entry or the new
/// one and never a partial write. The scratch file is removed on failure.
// Cost: O(n), n = data.len().
pub(crate) fn write_private(path: &Path, data: &[u8]) -> io::Result<()> {
    let scratch = scratch_path(path);
    let written = imp::create_private(&scratch, data).and_then(|()| fs::rename(&scratch, path));
    if written.is_err() {
        let _ = fs::remove_file(&scratch);
    }
    written
}

#[cfg(all(test, unix, not(miri), feature = "native"))]
mod tests {
    use super::*;
    use std::os::unix::fs::{MetadataExt, PermissionsExt};

    fn scratch_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "mutsu-private-files-{name}-{}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&dir);
        dir
    }

    #[test]
    fn creates_directories_and_entries_private() {
        let root = scratch_dir("modes");
        let dir = root.join("mutsu").join("precomp");
        ensure_private_dir(&root.join("mutsu")).unwrap();
        ensure_private_dir(&dir).unwrap();
        assert_eq!(fs::metadata(&dir).unwrap().mode() & 0o777, 0o700);
        assert_eq!(
            fs::metadata(root.join("mutsu")).unwrap().mode() & 0o777,
            0o700
        );
        let entry = dir.join("x.bin");
        write_private(&entry, b"data").unwrap();
        assert_eq!(fs::metadata(&entry).unwrap().mode() & 0o777, 0o600);
        assert_eq!(read_private(&entry).unwrap(), b"data");
        // A second write replaces the entry; no scratch file is left behind.
        write_private(&entry, b"next").unwrap();
        assert_eq!(read_private(&entry).unwrap(), b"next");
        let leftovers = fs::read_dir(&dir)
            .unwrap()
            .flatten()
            .filter(|e| e.path().extension().is_some_and(|x| x == "tmp"))
            .count();
        assert_eq!(leftovers, 0);
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn tightens_a_loose_directory_it_owns() {
        let root = scratch_dir("loose");
        fs::create_dir_all(&root).unwrap();
        fs::set_permissions(&root, fs::Permissions::from_mode(0o777)).unwrap();
        ensure_private_dir(&root).unwrap();
        assert_eq!(fs::metadata(&root).unwrap().mode() & 0o777, 0o700);
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn refuses_a_symlinked_directory() {
        let root = scratch_dir("dirlink");
        let real = root.join("real");
        fs::create_dir_all(&real).unwrap();
        let link = root.join("link");
        std::os::unix::fs::symlink(&real, &link).unwrap();
        assert!(ensure_private_dir(&link).is_err());
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn does_not_follow_a_symlinked_entry() {
        let root = scratch_dir("filelink");
        ensure_private_dir(&root).unwrap();
        let target = root.join("target.bin");
        write_private(&target, b"planted").unwrap();
        let entry = root.join("entry.bin");
        std::os::unix::fs::symlink(&target, &entry).unwrap();
        assert!(read_private(&entry).is_err());
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn refuses_an_entry_others_can_write() {
        let root = scratch_dir("writable");
        ensure_private_dir(&root).unwrap();
        let entry = root.join("entry.bin");
        fs::write(&entry, b"x").unwrap();
        fs::set_permissions(&entry, fs::Permissions::from_mode(0o666)).unwrap();
        assert!(read_private(&entry).is_err());
        fs::set_permissions(&entry, fs::Permissions::from_mode(0o644)).unwrap();
        assert_eq!(read_private(&entry).unwrap(), b"x");
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn does_not_write_through_a_planted_scratch_symlink() {
        let root = scratch_dir("scratchlink");
        ensure_private_dir(&root).unwrap();
        let victim = root.join("victim");
        fs::write(&victim, b"keep").unwrap();
        // The scratch name is exclusive: an existing file or symlink at it
        // makes the write fail instead of writing through it.
        let entry = root.join("entry.bin");
        let scratch = scratch_path(&entry);
        std::os::unix::fs::symlink(&victim, &scratch).unwrap();
        assert!(imp::create_private(&scratch, b"evil").is_err());
        assert_eq!(fs::read(&victim).unwrap(), b"keep");
        let _ = fs::remove_dir_all(&root);
    }
}
