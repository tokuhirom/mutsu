# Battery: Pod rendering — `Pod::To::Text`

**Slot:** Pod → plain text · **Chosen:** rakudo's own `Pod::To::Text`
(`rakudo/lib/Pod/To/Text.rakumod`, release 2026.06, Artistic-2.0) · **Kind:**
Adopted verbatim (BATTERIES.md §1 rung 1) · **Replaces:** a native Rust
reimplementation (rung 3).

## Status: bundled

The genuine upstream file ships at
`modules/Rakudo-Core/lib/Pod/To/Text.rakumod` and resolves with zero config:

```raku
use Pod::To::Text;
print pod2text($=pod);        # exported sub
print Pod::To::Text.render($=pod);   # class method
```

Output is character-for-character identical to `raku` on the same document —
including the trailing-newline behaviour the previous native renderer got wrong.

## Why it is vendored from rakudo rather than from Zef

`Pod::To::Text` is not an ecosystem distribution: rakudo ships it inside its own
core library. So the vendored copy comes from the rakudo release tarball, with
its `LICENSE` (Artistic-2.0) copied alongside it and the provenance recorded in
`modules/Rakudo-Core/README.md`. Nothing in the file is edited — the directory
exists precisely to hold upstream core modules unchanged.

## Why the native reimplementation was wrong

`pod2text` used to be a Rust builtin (`Interpreter::pod_to_text` in
`src/runtime/io_pod.rs`) with `use Pod::To::Text` recognized as a built-in no-op
module, introduced in #4541 to whitelist `roast/integration/advent2011-day10.t`.
It was written "the same pattern as JSON::Fast" — but that pattern was never
transferable. `JSON::Fast` genuinely cannot run here (the real distribution
needs ~50 nqp ops mutsu lacks); `Pod::To::Text` is **168 lines of plain Raku
with no `use` statements and no nqp dependency at all**. It only ever needed
rung 2 work, and only a little of it. See BATTERIES.md §1 and CLAUDE.md's ban on
native provision.

## What it took to run the real module (rung 2)

Three interpreter bugs, all general — none of them specific to Pod:

1. **A statement ending in a closure literal did not terminate.**
   `my &colored = sub ($text, $) { $text }` followed on the next line by
   `if %*ENV<POD_TO_TEXT_ANSI> { ... }` had the `if` absorbed as a postfix
   modifier, turning its block into a *bare* block that ran unconditionally.
   `expr_ends_with_block` (`src/parser/stmt/modifier.rs`) recognized
   `gather`/`do`/`try` and a call's trailing block argument, but not a bare
   `sub {...}` / `-> {...}` initializer. Pin: `t/closure-literal-ends-statement.t`.

2. **`$=pod` was collected out of heredoc bodies.** The collector scans source
   line by line with no notion of quoting, so a `=begin pod` (or a `#|`) written
   inside a `q:to/END/` string became a real `$=pod` entry — and the real
   `Pod::To::Text` then died walking Pod the program never declared. Heredoc
   bodies are now masked before both scanners
   (`src/runtime/io_pod_heredoc.rs`); this also subsumed a weaker duplicate in
   `io_doc.rs` that only understood bracket and slash delimiters, never `q:to"END"`.
   Pin: `t/pod-not-collected-from-heredoc.t`.

3. **A trailing-`}` statement's line numbers** — no change needed, but note that
   the masking blanks lines rather than dropping them precisely to keep
   declarator line numbers intact.

Remaining gap, deliberately not fixed here:
`todo/tickets/doc-init-pod-variable.md` (`DOC INIT` blocks still do not see
`$=pod`, because doing so exposes that a declarator's `WHEREFORE` is a type-name
placeholder rather than the routine object).

## Release gate

Not in `batteries.lock`: that harness fetches a *distribution's* upstream test
suite, and this module's tests live inside rakudo's own `t/`, which is not a
per-dist suite we can point the harness at. Coverage is `t/pod-to-text-bundled.t`
plus the two behavioural pins listed above, all of which run in the fatal `t/`
suite. Wiring rakudo's `t/02-rakudo/` selectively into the gate is possible
later.

## Other rakudo core modules — measured 2026-08-01

`Test`, `NativeCall`, `experimental`, `newline` and friends are ordinary Raku
files in `rakudo/lib/` too, so the obvious question is which one follows.
Measured, they are nothing like each other — **do not assume a provider is
retirable without measuring it**:

| module | lines | `nqp::` refs | distinct ops (missing) | parses on mutsu? | verdict |
| --- | ---: | ---: | --- | --- | --- |
| `Pod::To::Text` | 168 | 0 | 0 (0) | yes | **done** — this document |
| `newline` | 5 | 0 | 0 (0) | yes, and loads | moving it changes nothing: `$?NL` stays `Nil` either way, because the `package EXPORT::crlf { BEGIN OUR::<$?NL> := ... }` export mechanism is unimplemented |
| `Test` | 953 | 90 | 11 (9) | yes, **and loads** | **strongest candidate** — `todo/tickets/vendor-real-test-module.md` |
| `experimental` | 260 | 10 | 6 (4) | **no** | same parser gap as `NativeCall::Types`, plus `nqp::getcomp` (a compiler object) |
| `NativeCall` | 1483 | 308 | 76 (61) | **no** | **not retirable** — `use QAST:from<NQP>` + MoarVM dispatch programs; `todo/deep/nativecall-cannot-be-vendored.md` |

What matters is not line count but *what kind* of ops are missing. `Test` needs
nine thin ones (`getstdout`, `getstderr`, `setbuffersizefh`, `can`, `eqaddr`,
`join`, `split`, `time`, `time_n`); `NativeCall` needs sixty-one that reach into
the VM's dispatch machinery and object representation.

Also in `rakudo/lib/` with zero `nqp::` references and no provider here at all —
candidates to *add* rather than retire: `RakuDoc::To::Text` (319 lines),
`RakuDoc::To::RakuDoc` (221), `CompUnit::Repository::Staging` (82).
