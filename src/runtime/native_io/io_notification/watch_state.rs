//! The per-entry snapshot the `watch-path` watcher polls, and the diff
//! between two snapshots that becomes its events (see the parent module).

use std::collections::BTreeMap;
use std::ffi::OsString;
use std::fs;
use std::path::Path;
use std::time::SystemTime;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum ChangeKind {
    Changed,
    Renamed,
}

impl ChangeKind {
    pub(super) fn key(self) -> &'static str {
        match self {
            ChangeKind::Changed => "FileChanged",
            ChangeKind::Renamed => "FileRenamed",
        }
    }
}

/// What a snapshot remembers about one filesystem object: enough to notice a
/// content write (size, mtime), a permission change (mode) and a replacement
/// under the same name (inode).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) struct Stamp {
    len: u64,
    modified: Option<SystemTime>,
    readonly: bool,
    /// A non-empty regular file whose last inode change was a content write
    /// (ctime == mtime). A *new* entry like that was created and then written
    /// between two polls, which libuv reports as `FileRenamed` followed by
    /// `FileChanged`; one moved in by `rename` (ctime > mtime) or created
    /// empty is `FileRenamed` alone.
    content_written_last: bool,
    #[cfg(unix)]
    mode: u32,
    #[cfg(unix)]
    ino: u64,
}

impl Stamp {
    fn of(meta: &fs::Metadata) -> Self {
        #[cfg(unix)]
        use std::os::unix::fs::MetadataExt;
        #[cfg(unix)]
        let content_written_last = meta.is_file()
            && meta.len() > 0
            && (meta.ctime(), meta.ctime_nsec()) == (meta.mtime(), meta.mtime_nsec());
        // No portable ctime: a new entry is reported as `FileRenamed` alone.
        #[cfg(not(unix))]
        let content_written_last = false;
        Stamp {
            len: meta.len(),
            modified: meta.modified().ok(),
            readonly: meta.permissions().readonly(),
            content_written_last,
            #[cfg(unix)]
            mode: meta.mode(),
            #[cfg(unix)]
            ino: meta.ino(),
        }
    }
}

/// One poll's view of the watched path.
#[derive(Clone, PartialEq, Eq, Debug)]
pub(super) enum WatchState {
    /// A file (or anything that is not a directory); `None` while it is absent.
    File(Option<Stamp>),
    /// A directory: its own stamp plus one per entry, keyed by name. A
    /// `BTreeMap` so a poll that sees several entries change reports them in
    /// a stable (name) order.
    Dir {
        own: Stamp,
        entries: BTreeMap<OsString, Stamp>,
    },
}

impl WatchState {
    // Cost: O(n log n), n = entries of the watched directory (one `lstat` each).
    pub(super) fn snapshot(path: &Path) -> Self {
        let Ok(meta) = fs::metadata(path) else {
            return WatchState::File(None);
        };
        if !meta.is_dir() {
            return WatchState::File(Some(Stamp::of(&meta)));
        }
        let mut entries = BTreeMap::new();
        if let Ok(read_dir) = fs::read_dir(path) {
            for entry in read_dir.flatten() {
                // `lstat`, so replacing a symlink's target is not reported as
                // a change of the link itself (libuv watches the entry too).
                if let Ok(entry_meta) = fs::symlink_metadata(entry.path()) {
                    entries.insert(entry.file_name(), Stamp::of(&entry_meta));
                }
            }
        }
        WatchState::Dir {
            own: Stamp::of(&meta),
            entries,
        }
    }

    /// The events that turn `self` into `next`, as `(path, kind)` pairs.
    // Cost: O(n), n = entries in the two snapshots (a merge of sorted maps).
    pub(super) fn diff(&self, next: &WatchState, display: &str) -> Vec<(String, ChangeKind)> {
        let mut events = Vec::new();
        match (self, next) {
            (WatchState::File(before), WatchState::File(after)) => {
                if before != after {
                    let kind = if before.is_some() && after.is_some() {
                        ChangeKind::Changed
                    } else {
                        ChangeKind::Renamed
                    };
                    events.push((display.to_string(), kind));
                }
            }
            (
                WatchState::Dir {
                    own: own_before,
                    entries: before,
                },
                WatchState::Dir {
                    own: own_after,
                    entries: after,
                },
            ) => {
                for (name, stamp) in before {
                    match after.get(name) {
                        None => events.push((entry_path(display, name), ChangeKind::Renamed)),
                        Some(now) if now != stamp => {
                            events.push((entry_path(display, name), ChangeKind::Changed))
                        }
                        Some(_) => {}
                    }
                }
                for (name, stamp) in after {
                    if !before.contains_key(name) {
                        events.push((entry_path(display, name), ChangeKind::Renamed));
                        if stamp.content_written_last {
                            events.push((entry_path(display, name), ChangeKind::Changed));
                        }
                    }
                }
                // Adding or removing an entry also touches the directory's own
                // mtime; only a change the entries do not explain (a chmod of
                // the directory) is reported against the directory itself.
                if events.is_empty() && own_before != own_after {
                    events.push((display.to_string(), ChangeKind::Changed));
                }
            }
            // The watched path changed kind (a file replaced by a directory,
            // or a watched directory removed).
            _ => events.push((display.to_string(), ChangeKind::Renamed)),
        }
        events
    }
}

/// The path rakudo reports for an entry of a watched directory: the watched
/// path as written, joined with the entry name (`tmp/w/` + `x` = `tmp/w/x`).
fn entry_path(display: &str, name: &OsString) -> String {
    let name = name.to_string_lossy();
    if display.ends_with('/') || display.is_empty() {
        format!("{display}{name}")
    } else {
        format!("{display}/{name}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stamp(len: u64) -> Stamp {
        Stamp {
            len,
            modified: None,
            readonly: false,
            content_written_last: false,
            #[cfg(unix)]
            mode: 0o644,
            #[cfg(unix)]
            ino: 1,
        }
    }

    fn dir(entries: &[(&str, u64)], own_len: u64) -> WatchState {
        WatchState::Dir {
            own: stamp(own_len),
            entries: entries
                .iter()
                .map(|(n, l)| (OsString::from(n), stamp(*l)))
                .collect(),
        }
    }

    #[test]
    fn directory_entries_created_changed_and_removed() {
        let before = dir(&[("a", 1), ("b", 1)], 0);
        let after = dir(&[("a", 2), ("c", 1)], 1);
        assert_eq!(
            before.diff(&after, "d"),
            vec![
                ("d/a".to_string(), ChangeKind::Changed),
                ("d/b".to_string(), ChangeKind::Renamed),
                ("d/c".to_string(), ChangeKind::Renamed),
            ]
        );
    }

    #[test]
    fn directory_own_change_is_reported_only_when_no_entry_explains_it() {
        let before = dir(&[("a", 1)], 0);
        assert_eq!(
            before.diff(&dir(&[("a", 1)], 1), "d/"),
            vec![("d/".to_string(), ChangeKind::Changed)]
        );
        assert_eq!(
            before.diff(&dir(&[("a", 1), ("x", 0)], 1), "d/"),
            vec![("d/x".to_string(), ChangeKind::Renamed)]
        );
    }

    #[test]
    fn a_new_entry_written_since_the_last_poll_is_renamed_then_changed() {
        let before = dir(&[], 0);
        let mut written = stamp(4);
        written.content_written_last = true;
        let after = WatchState::Dir {
            own: stamp(1),
            entries: [
                (OsString::from("w"), written),
                (OsString::from("m"), stamp(4)),
            ]
            .into_iter()
            .collect(),
        };
        assert_eq!(
            before.diff(&after, "d"),
            vec![
                ("d/m".to_string(), ChangeKind::Renamed),
                ("d/w".to_string(), ChangeKind::Renamed),
                ("d/w".to_string(), ChangeKind::Changed),
            ]
        );
    }

    #[test]
    fn watched_file_write_and_removal() {
        let file = WatchState::File(Some(stamp(1)));
        assert_eq!(
            file.diff(&WatchState::File(Some(stamp(2))), "f"),
            vec![("f".to_string(), ChangeKind::Changed)]
        );
        assert_eq!(
            file.diff(&WatchState::File(None), "f"),
            vec![("f".to_string(), ChangeKind::Renamed)]
        );
        assert!(file.diff(&file.clone(), "f").is_empty());
    }
}
