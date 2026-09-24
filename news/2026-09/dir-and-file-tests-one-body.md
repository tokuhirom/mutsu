# `dir` / `IO::Path.dir` and the file tests / `~~ :r` each have one body

`dir` and `IO::Path.dir` were two copies of the directory listing:

- the method returned a `List` where the sub returned a `Seq`;
- both died with an `X::AdHoc` ("Failed to read dir ...") where rakudo throws `X::IO::Dir` with
  "Failed to get the directory contents of '<absolute>': Failed to open dir: <reason>".

`Interpreter::dir_listing` is now the one body, and both forms call it (ADR-0118 §2.7). The fix
also exposed that `io_exception_error` never put its message on the exception object, so a caught
IO error's `.message` was empty. It now carries the message, as `io_exception_failure` already
did.

The file tests `.e`/`.f`/`.d`/`.l`/`.r`/`.w`/`.x`/`.rw`/`.rwx`/`.s`/`.z` had three copies: the
method, the VM smartmatch `$path ~~ :r`, and the interpreter smartmatch. The two smartmatch
copies tested the mode bits instead of `access(2)`, so on a mode-000 file root's
`$z ~~ :rw` was False while `$z.rw` was True. They also tested `rw`/`rwx`/`s`/`z`/`l` against the
path string without resolving it against the IO::Path's `CWD`. `native_io::io_file_test` is now
the one body.

`t/io/dir-and-file-test-one-body.t` pins 17 rows. Eight of them failed before this change.
