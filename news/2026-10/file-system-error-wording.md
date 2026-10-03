# File-system errors use Rakudo's wording

A missing or unreadable file used to report mutsu's own text, different on
every route: `Failed to slurp 'nope.txt': No such file or directory (os error 2)`,
`Failed to read 'nope.txt': ...`, a bare `No such file or directory (os error 2)`
from `Grammar.parsefile`. Every file routine now reports what Rakudo reports
(#9878):

- opening a file (`slurp`, `open`, `spurt`, `IO::Path.lines`/`.words`,
  `EVALFILE`, `Grammar.parsefile`) dies with
  `Failed to open file <absolute path>: <strerror text>`;
- slurping a directory says `Tried to open directory <absolute path>`, and
  `open` of a directory fails with `X::IO::Directory`;
- file tests (`.d`, `.f`, `.s`, `.modified`, ...) name the absolute path in
  their `X::IO::DoesNotExist`;
- `copy`, `rename`, `move`, `rmdir`, `chmod` and `mkdir` fail with their
  `X::IO::*` exception, carrying the `from`/`to`/`path`/`os-error`
  attributes and libuv's wording (`Failed to copy file: illegal operation on
  a directory`).

The spellings live in one module, `src/runtime/native_io/fs_errors.rs`, and
the `slurp`, `spurt`, `copy` and `rename`/`move` bodies that the routine and
the `IO::Path` method each carried are now one implementation each
(`native_io/fs_ops.rs`). Along the way `Grammar.parsefile` and `EVALFILE`
started resolving a relative path against `$*CWD`, `spurt :createonly` became
a single `O_EXCL` open instead of a check-then-create race, and an `open`ed
handle's `.path` is the path as written, as in Rakudo.
