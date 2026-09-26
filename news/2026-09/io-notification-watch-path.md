# `IO::Notification.watch-path` and real `IO::Notification::Change` events

`IO::Notification.watch-path($path)` now exists, and `IO::Path.watch` is its
method form (#9586). Both return a live `Supply` of `IO::Notification::Change`
objects with `.path`, `.event`, `.IO` and a `path: event` gist, and the
`FileChangeEvent` enum (`FileChanged`, `FileRenamed`) is in core.

Before, `watch-path` died with "No such method", and `IO::Path.watch` was a
10 ms poll of the watched path's own metadata that emitted the path as a plain
`Str`: for a directory it could not say *which* entry changed, so App::Lorea's
`watch-recursive` (built on `watch-path`) never saw the file change it
provoked.

The watcher now polls a per-entry snapshot and diffs consecutive snapshots,
following rakudo's libuv mapping: an entry appearing or disappearing is
`FileRenamed`, a changed entry (size, mtime, mode, inode) is `FileChanged`, and
`.path` is the watched path joined with the entry name (`tmp/w` + `x` is
`tmp/w/x`; `IO::Path.watch` reports under the absolute path, as rakudo does).
A watch on a missing path quits with `no such file or directory`. Polling
rather than inotify/kqueue keeps one dependency-free implementation on every
release target; the watcher starts reporting once the Supply is tapped and
retires as soon as its taps are closed or dropped, so a quiet directory no
longer pins a thread.

`IO::Path.watch`, `IO::Notification.watch-path` and the event's `.IO`/`.gist`
are dispatched natively by the VM, sharing one implementation with the
interpreter's native-class path (the old `watch` arm went through the
instance native-method fallback).
