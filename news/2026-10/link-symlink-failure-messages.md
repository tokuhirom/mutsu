# `link` / `symlink` failures read like Rakudo's

A failed `link` or `symlink` (the subs and the `IO::Path` methods) built its
`X::IO::Link` / `X::IO::Symlink` message from Rust's `io::Error` text and the
paths as written: `Failed to create hard link 'f.txt' for target 'f.txt': File
exists (os error 17)`. All four now go through the shared `fs_syscalls`
`hard_link` / `symlink` that `nqp::link` / `nqp::symlink` already used, and
report Rakudo's `Failed to create link called '/abs/f.txt' on target
'/abs/f.txt': Failed to link file: file already exists`, with absolute `target`
and `name` attributes and the libuv-worded `os-error` (#11737).
