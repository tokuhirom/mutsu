# The `nqp::` process ops rakudo's real `Test.rakumod` needs

mutsu provides `Test` natively (`src/runtime/test_functions/`), which is
BATTERIES.md rung 3 — a private dialect of a module that upstream ships as
ordinary Raku source. `todo/tickets/vendor-real-test-module.md` measured what
stands between mutsu and running the genuine `rakudo-2026.06/lib/Test.rakumod`
verbatim: the file already parses and loads, and the only thing it needs is a
handful of thin `nqp::` ops. This lands them.

Implemented in a new `src/runtime/nqp_ops_process.rs` (the process /
introspection half of the value-op table; `nqp_ops.rs` keeps the arithmetic,
buffer and file-handle ops and was at the 500-line limit):

| op | what it is |
| --- | --- |
| `getstdout` / `getstderr` / `getstdin` | the **process** standard handles, found by target in the VM's handle table rather than through `$*OUT`/`$*ERR`/`$*IN` — nqp code reaches for them precisely to bypass a dynamic override |
| `setbuffersizefh` | set a handle's output buffer capacity (`0` = unbuffered), the same state Raku's `$fh.out-buffer = $n` sets, so pending bytes are flushed first |
| `time` | wall clock as integer **nanoseconds** since the epoch (MoarVM's `time`, which replaced the float-valued `time_n`) |
| `eqaddr` | object identity as an int `0`/`1` — the relation `=:=` already implements |
| `can` | int `0`/`1`: does this object have that method (the low-level `$obj.^can`) |
| `join` / `split` | literal join/split over an nqp list, no regex and no Raku `split` adverbs |

`can`, `join`, `split` and `time` each collide with a same-named Raku builtin of
*different* semantics, so they are real full-name `nqp::` arms rather than a
relaxation of the aliasing guard in `builtins_operators_fallback.rs` — the guard
that made an unimplemented op fail loudly instead of silently answering with
Raku semantics stays exactly as strict as it was.

One parser-side fix came with them: a no-paren zero-argument `nqp::` term used
to be special-cased for `nqp::gethostname` alone, so bare `nqp::time` — which
`Test.rakumod` writes on 40-odd lines — raised "Could not find symbol '&time' in
'nqp'". The whole reserved `nqp::` namespace now dispatches through the op
table in term position; an op mutsu does not implement still fails loudly.

With these, the unmodified upstream `Test.rakumod` runs on mutsu:

```
$ mutsu -I <dir> -e 'use Test2; plan 3; ok 1, "ok works"; is 2+2, 4, "is works"; is "a", "a", "str"'
1..3
ok 1 - ok works
ok 2 - is works
ok 3 - str
```

(`Test2` is the upstream file with only `unit module Test;` renamed, to bypass
mutsu's `use Test` interception.) Pinned by `t/nqp-process-ops.t`. The ops are
independently useful — `getstdout`/`getstderr`/`eqaddr`/`can` show up in other
real distributions — and swapping the `Test` implementation itself is
deliberately left to later steps of the ticket.
