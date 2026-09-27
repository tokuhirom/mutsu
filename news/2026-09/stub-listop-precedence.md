# Stub listops `...`/`!!!`/`???` parse at list-prefix precedence

Found by the doc-diff sweep (issue #9780). The stub listops (`operators.rakudoc`'s
`listop C«...»`/`!!!`/`???`) sit at "list prefix precedence": tighter than the
loose word-logicals (`and`/`or`/...) and optionally argument-less. mutsu's
parser instead read their optional message with the full statement-level
`expression()`:

- `... if $x; say "ok"` failed to parse — the parser tried to read `if $x` as
  the stub's message expression instead of recognizing the trailing statement
  modifier.
- `??? $x or say "c"` bound too loosely — `$x or say "c"` was swallowed whole
  as the message, so `say "c"` never ran independently of the stub.

Both `...`/`!!!`/`???` now share one message parser
(`parse_stub_message` in `parser/primary/regex/lit.rs`) that recognizes a
statement modifier, a closing bracket, or a loose word-logical as "no
argument" and otherwise parses the message at `expression_no_word_logical`'s
precedence (looser than the comma, tighter than `and`/`or`/...), matching how
the stub listops actually bind.

Fixing the precedence exposed a second, latent bug: `builtin_stub_warn`
(`???`'s runtime handler) resolved its warning by returning a bare
`RuntimeError::warn_signal`, relying on the top-level VM loop's `is_warn()`
recovery to push nothing in its place — harmless when `???` was always a bare
statement (the following `SinkPop` tolerates an empty stack), but a hard VM
panic (`Dup` on an empty stack) once `???` could appear as `or`'s left
operand. It now resolves inline via `raise_resumable_warning`, the same path
`warn` itself uses, returning `Value::NIL` so `??? $x or B` runs `B` exactly
as raku does.

Pinned by `t/lang/operators/yada-operator-precedence.t`.
