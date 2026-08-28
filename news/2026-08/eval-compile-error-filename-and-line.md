# A compile-time exception raised inside EVAL now carries `.line` and `.filename`

Rakudo's `X::Comp`/`X::Syntax::*` family exposes `.line` and `.filename` on a
compile-time diagnosis — the line within, and the pseudo-file name of, the
source that failed to compile. When that source is a string passed to
`EVAL`, rakudo reports the EVAL's own synthesized name (matching `/EVAL/`)
and the line within the EVAL'd string, not the caller's line or file.

mutsu had two separate gaps that both surfaced on
`X::Syntax::Pod::BeginWithoutIdentifier` (`=begin` with no block name):

- **`.filename` did not exist as a method at all** on any exception. Only a
  looser `.file` 0-arg accessor was implemented, generically, for every
  `X::`/`CX::`/`Exception` instance. Added `.filename` alongside it
  (`src/builtins/methods_0arg/mod.rs`), reading a `filename` attribute with
  a fallback to `file` for symmetry.
- **`.line` was never populated** for this exception (and any other built
  via `PError::fatal_with_exception` without a source position). The
  builder never recorded how much of the source remained unconsumed at the
  failure point (`PError::remaining_len`), so `parser::parse_program`'s
  fatal-error branch — which computes line/column from exactly that field —
  never reached its `err.set_line`/`err.set_column` calls, and even when it
  did, that computed line was never copied onto the pre-built exception's
  own attributes (only the untyped-fallback exception-construction path did
  that). Fixed both: `pod_begin_without_identifier_error` now anchors its
  reported position at the `=begin` construct's own start (not the
  remainder after it — the shared line/column math always skips forward
  over trailing whitespace looking for "the next real token", which for
  this diagnosis' typically-all-whitespace remainder walked straight past
  the newline ending the `=begin` line and reported one line too far down;
  verified against `raku`: `EVAL "=begin\nfoo\n=end\n"` still answers
  `.line == 1`), and `parser::parse_program`'s fatal branch now copies the
  computed `line`/`column` onto any pre-built exception that does not
  already carry them.

`.filename` itself needs the EVAL unit's synthesized name (`EVAL_0`, …),
which only `builtin_eval` (`src/runtime/builtins_eval_misc.rs`) knows —
so it backfills `filename`/`file` on any exception a parse-coded `EVAL`
error carries, covering the whole `X::Comp` family raised while parsing an
EVAL'd string, not just this one exception class (verified with a sibling,
`X::Syntax::Malformed`).

This was found and fixed while closing out
`roast/S32-exceptions/misc2.t`'s `MUTSU_REAL_TEST=1` residue (the real
vendored `Test.rakumod`'s `throws-like` calls `$ex.filename`/`$ex.line` as
real Raku methods, unlike mutsu's native `throws-like`, which only checked
these via its own parallel special-case code). Pinned by
`t/eval-compile-error-line-and-filename.t`, green under `raku` too.
