# A compile-time diagnosis's `$!.backtrace` is a real `Backtrace` again, stamped `is-runtime => False`

Split off from `news/2026-08/exception-attributes-missing-for-throws-like.md`,
whose "Also worth doing" footnote this was.

```raku
try { EVAL q[my $0] };
say $!.backtrace.^name;      # raku: Backtrace     mutsu (before): Str
say $!.backtrace.is-runtime; # raku: False         mutsu (before): dies
```

`Backtrace.is-runtime` distinguishes a backtrace captured while the program was
running from one describing a compilation failure. Both runtime builders learned
to stamp it in
`news/2026-08/throws-like-named-matchers-no-longer-silently-skipped.md`; the
compile-time side never built a `Backtrace` to stamp.

## Root cause — not quite what the ticket assumed

The ticket's hypothesis was that the compile-time path "carries its backtrace as
a plain string". It does not carry one *at all*: a compile-time diagnosis
reaches `$!` with `err.backtrace()` empty, and the native `.backtrace` accessor
in `builtins/methods_0arg` falls back to `Value::str(String::new())`. The empty
string is defined and truthy, so `.defined` answered True and nothing looked
obviously broken — but `.^name` was `Str` and `.is-runtime` was unaskable.

There was a second, independent obstacle:
`RuntimeError::exception_value_with_backtrace` returned a *structured*
exception unchanged, discarding the `backtrace` argument entirely. Every
compile-time diagnosis is structured (`X::Syntax::Variable::Numeric` here), so
the argument could never have reached it. It now stamps the supplied backtrace
onto a structured exception that has none, and leaves one that already carries
its own (stamped at the throw site) alone.

## Fix

`vm_try_catch_ops`'s `$!` construction now asks whether the exception does
`X::Comp` — rakudo's compile-time-diagnosis role, which mutsu's registry already
composes onto the whole `X::Syntax::*` family. When it does, the `Backtrace` is
built with `is-runtime => False`, from the live routine stack of the code that
*triggered* the compilation (the `EVAL`, the `use`). That is exactly the
non-setting frame rakudo's own compile-time backtrace ends with — rakudo also
lists ~30 `Perl6::Grammar` frames above it, which mutsu deliberately does not
chase (see `todo/tickets/backtrace-frame-indexing-returns-nil.md`: mutsu has no
Raku-written CORE setting, so matching rakudo's frame *count* is architecturally
invasive and would make the output less useful).

Both `Backtrace` builders in `vm/vm_helpers.rs` gained an explicit `is_runtime`
parameter rather than hard-coding `True`.

## Pin

`t/exception-rendering-and-phasers.t` — `$!.backtrace.^name` is `Backtrace` and
`.is-runtime` is False for a compile-time diagnosis, True for a runtime one.
Verified against real `raku`.

## Files

- `src/vm/vm_helpers.rs` — `build_backtrace_value_with_runtime`,
  `backtrace_value_from_string_with_runtime`
- `src/vm/vm_try_catch_ops.rs` — the `X::Comp` decision
- `src/value/error_construct.rs` — `exception_value_with_backtrace` now stamps a
  structured exception that carries no backtrace of its own
