# Acme::Anguish goes green: three interpreter gaps fixed

`Acme::Anguish` (a small invisible-Unicode esolang interpreter) and its
`Test::Output` dependency exposed three unrelated bugs, all found chasing one
distribution's own test suite from `red`/`partial` to `green`:

**Parser: `|<...>` after a forward-referenced sub.** `sub-name |<a b>, ...`
(no parens) is a listop call with a flattened `<...>` word-list argument.
When the callee is declared *later* in the same scope — a legal Raku forward
reference — the parser had not yet registered its name, and `starts_slip_prefix_arg`
recognized `|$x`/`|@x`/`|%x`/`|&x`/`|(...)` as unambiguous slip-prefix
argument starts but not `|<...>`, so `test |<all is>, ...` parsed as the
infix any-junction operator over a bare 0-arg term instead. Test::Output's
`output-is`/`stdout-is`/etc. all call `test |<all is>, &?ROUTINE.name, |@args`
with `test` declared further down the same module.

**VM: `$buf[i]++`/`--` silently lost the write.** The increment/decrement
read-modify-write path had no Buf/Blob-aware branch, unlike plain element
assignment (which already writes through the native storage node). Acme::Anguish's
esolang interpreter builds its tape with `$stack[$ptr]++`.

**VM: `$PROCESS::OUT = ...` didn't sync with `$*OUT`.** The double-colon-qualified
spelling of a built-in dynamic variable mapped only to the sigilless `*OUT`
key, leaving `$*OUT` — what `print`/`say` reads to find their destination —
stale at the original handle. Both spellings are seeded together at startup
and are meant to be synonyms; the write now mirrors both, the same way a
direct `$*OUT`/`*OUT` write already did. Test::Output's `capture` idiom
reassigns `$PROCESS::OUT`/`$PROCESS::ERR` around the code under test.

Each fix is pinned by a focused regression test under `t/`. The distribution's
own suite (`t/00-use.t`, `t/01-output.t`, `t/02-input-test.t`, `t/meta.t`) now
passes in full — `ecosystem/dists/A/Acme--Anguish~4a941d7a.json` goes
`partial` (2/4 files) → `green` (4/4 files).

`t/meta.t`'s earlier debug-build timeout (License::SPDX/JSON::Class parsing a
~330KB JSON license list) was `debug_assert_eq!`-driven interpreter overhead,
not an algorithmic bug: it passes in 28s on a release binary, matching
`docs/agent-environments.md`'s guidance to confirm a heavy-file timeout on
`target/release/mutsu` before treating it as a regression.
