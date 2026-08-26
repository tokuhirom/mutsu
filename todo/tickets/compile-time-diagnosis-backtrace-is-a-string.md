# A compile-time diagnosis's `$!.backtrace` is a `Str`, not a `Backtrace`

Split off from `news/2026-08/exception-attributes-missing-for-throws-like.md`,
whose "Also worth doing" footnote this was. The ten *attribute* gaps that
ticket tracked are all closed; this one is a different shape of problem and
was left out of that PR deliberately.

## What is happening

mutsu attaches a real `Backtrace` object only on the runtime path. A
compile-time diagnosis carries its backtrace as a plain string, so the object
answers no `Backtrace` methods at all:

```raku
try { EVAL q[my $0] };
say $!.backtrace.^name;      # raku: Backtrace     mutsu: Str
say $!.backtrace.defined;    # raku: True          mutsu: True
say $!.backtrace.is-runtime; # raku: False         mutsu: dies -- "No such method
                             #                     'is-runtime' for invocant of type 'Str'"
```

`Backtrace.is-runtime` itself exists (added in
`news/2026-08/throws-like-named-matchers-no-longer-silently-skipped.md`, where
the two runtime `Backtrace` builders learned to stamp the flag) — it is the
compile-time side that never builds a `Backtrace` to stamp.

Nothing depends on it today: a `Str` is truthy and `.is-runtime` is only ever
asked in a boolean position, where the honest `False` and the current
"unaskable" both read falsy in the tests that exercise it. So this is a
correctness/shape gap rather than a live failure.

## Why it is not a one-liner

The string backtrace is produced where a `PError` is promoted into a
`RuntimeError`, well away from the `Backtrace` builders, and several call
sites read `err.backtrace` expecting text (including the `--` CLI display and
`exception_value_with_backtrace`'s "legacy error that only carries its
backtrace as a string" path). The fix is to build a real `Backtrace` for the
compile-time path — carrying `is-runtime => False`, and a single frame naming
the diagnosis's file and line, which the parse-error metadata already has —
and then move those readers onto it.

## Affected files (starting point)

- `src/value/error_construct.rs` — `exception_value_with_backtrace`, which
  currently wraps a legacy string backtrace as-is.
- Wherever the runtime `Backtrace` builders stamp `is-runtime` (grep
  `is-runtime`); the compile-time path needs the mirror of that.
- `src/parser/mod.rs` — `parse_program`'s `RuntimeError` mapping, which already
  computes the `line`/`column`/`pre`/`post` metadata a compile-time frame would
  need.
