# `use Module::Name` intermittently fails to resolve a module that IS on the `-I` search path, depending on unrelated `~/.cache/mutsu/precomp` state

## TL;DR

With several `-I` search paths given on the command line, `use
Cro::Policy::Timeout` (a role declared in
`.../Cro-Core/lib/Cro/Policy/Timeout.rakumod`, reachable via one of the `-I`
paths) intermittently fails with `Could not find Cro::Policy::Timeout in:
(module repositories)`, even though the exact same `-I` flag set succeeds on
a different invocation. The failure/success outcome is **not** a pure
function of the `-I` flags — the SAME flags flip between reliably-passing
and reliably-failing across different shell sessions/batches, and the
outcome empirically tracks the presence/contents of the global,
user-wide `~/.cache/mutsu/precomp/` directory rather than anything about the
invocation itself.

## Evidence

Using `target/release/mutsu` (release build, current as of `65844e560` +
in-flight ADR-0023 branch — confirmed unrelated to that branch's changes,
since module resolution is a compile-time `use`-statement concern with no
overlap with ADR-0023's runtime for-loop/thread capture code):

```
BIN=target/release/mutsu
HTTP=.../C_RO_CRO_HTTP_.../lib
CORE=.../C_RO_CRO_CORE_.../lib
TLS=.../C_RO_CRO_TLS_.../lib
JWT=.../J_SO_JSON_JWT_.../lib

$BIN -I $HTTP -I $CORE -I $TLS -e 'use Cro::Policy::Timeout; say "ok"'
# sometimes: ok
# sometimes (same flags, different run): Could not find Cro::Policy::Timeout in: (module repositories)
```

Observed session (in order):
1. Fresh clean release build, 4-flag set (`HTTP CORE TLS JWT`): fails 3/3.
2. `rm -rf ~/.cache/mutsu/precomp`, same 4-flag set: still fails.
3. 3-flag set (`HTTP CORE TLS`, no JWT): passes 5/5.
4. Re-tried the SAME 4-flag set from step 1 a few commands later (no source
   or binary change in between): passes 3/3.
5. Pairwise `CORE + JWT` / `JWT + CORE` / `JWT` alone: `JWT` alone fails
   (expected — `Cro::Policy::Timeout` isn't in JWT's lib), but `CORE + JWT`
   (either order) passes.
6. A 4-path set substituting a different, unrelated 4th path (`TinyFloats`
   instead of `JWT`) passed immediately.

No single flag or flag count was found to be a reliable, reproducible
trigger by itself — every "this flag/count breaks it" hypothesis was
falsified by a later run with identical inputs succeeding. This points at
**state outside the process's command-line arguments** — most likely
something in the on-disk precompilation cache
(`~/.cache/mutsu/precomp/`, see `src/precomp.rs`) or the module-name → file
resolution step that builds/consults some index ahead of or alongside it —
rather than a pure argument-handling bug.

## Why this matters

This is the thing that made ADR-0023's acceptance criterion #4 (`Cro`
`http-session-inmemory`/`http-session-persistent` subtests 8-9, which need
the full ~9-entry `-I` list from `tmp/cro-work/inc-paths.txt`) impossible to
verify reliably in this session — the harness itself intermittently can't
even load `Cro::HTTP::Client` (which `use`s `Cro::Policy::Timeout`), unrelated
to whatever the test is actually checking.

## Discovery context

Found while trying to run the Cro `http-session-*` rakutest suites for
ADR-0023 (`docs/adr/0023-binding-provenance-spawn-capture.md`) verification.
Not investigated further within that session (time-boxed) since it is
unrelated to that ADR's actual mechanism (module `use` resolution happens
entirely before any of the runtime for-loop/thread-spawn code the ADR
touches) — ADR-0023 was independently verified via minimal `raku`-vs-`mutsu`
repros, a new roast-independent pin test
(`t/for-loop-param-start-sibling-isolation.t`), the full `t/` local TAP
suite, and the specific concurrency regression tests, none of which touch
this module-loading path.

## Next steps for whoever picks this up

- Read `src/precomp.rs`'s cache-key construction (keyed by a hash of the
  *canonical resolved file path*, per its own doc comment, which should be
  stable across different `-I` sets resolving the SAME file — so the bug may
  be upstream of the precomp cache, in whatever step resolves a module NAME
  to a canonical PATH across multiple `-I` directories) and
  `src/runtime/run_modules.rs` / `resolution.rs` for any secondary,
  persistent (on-disk) module-name → path index.
- Reproduce with `MUTSU_TRACE` (module/precomp categories, if any) enabled to
  see the actual search sequence on a failing vs. passing run with byte-identical
  arguments.
- Check for a stale/torn write: is there a repository-index file written
  non-atomically that a concurrent invocation (or a previous run under
  different `-I` flags) could leave in a partially-written state read by a
  later run? The observed "same input, different outcome" pattern is a
  classic caching/atomicity smell.
- Once root-caused, add a regression test exercising ≥3 sibling `-I`
  directories with a module in a non-first one.

## Verification (once fixed)

- The commands in "Evidence" above should give `ok` deterministically, run
  after run, with no dependency on `~/.cache/mutsu/precomp`'s prior state.

## 2026-08-10 follow-up: two independent attempts to reproduce, both inconclusive — read before spending more time here

**Background-agent investigation (~85 runs, could not reproduce):** a
dedicated investigation session ran the exact "Evidence" repro (and the
heavier `Cro::HTTP::Client` chain, closer to the original
`Cro::Policy::Timeout` symptom) roughly 85 times — sequential batches of
5/10/20, 10-12x concurrent parallel launches, cold and warm
`~/.cache/mutsu/precomp`, and a from-scratch `cargo build --release`
rebuild (byte-identical via sccache, ruling out a stale binary) — and got
`ok` every single time, zero failures. It traced both `resolve_module_path`
(`src/runtime/run_modules.rs:7-108`, a pure deterministic filesystem walk,
no shared cache) and `MODULE_SCAN_CACHE` (`src/parser/stmt/simple/module_exports.rs:21-26`,
correctly keyed by resolved file path) and found nothing unsound. It flagged
a plausible **prior** cause: `ParseMemo` (`src/parser/memo.rs`) used to key
memo entries by raw `(ptr, len)` of the input `&str`, which could return a
stale entry for unrelated input when a short-lived nested-parse buffer got
freed and its address reused (see
`todo/deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md`
for the proven, identical-signature case: same input/`-I` set flipping pass
↔ fail across rebuilds). That hole was closed in commit `490dc01a4` (PR
#6132, merged 2026-08-09) by mixing a per-parse-call generation counter into
every memo key — confirmed to be an ancestor of both this ticket's base
commit and current `HEAD`, so it cannot be the (whole) explanation, but the
failure mode class matches closely enough that a *residual* variant is worth
suspecting if this recurs.

**A separate same-session direct reproduction turned out to be a shell
artifact, not a mutsu bug — a critical confound to rule out first.** Testing
independently (not knowing about the agent's negative result yet), a
combined `INC="-I path1 -I path2 -I path3 ..."` string (as built by
`tmp/cro-suite-run.sh` into one shell variable, one `-I` per Cro
dependency) reproduced "Could not find Cro::Message" **deterministically,
5/5**, when expanded unquoted directly in an interactive **zsh** shell
(`$BIN $INC -I ... -e '...'`). Wrapping the *exact same* command in
`bash -c '...'` made it pass **5/5**. Root cause: zsh does not word-split an
unquoted variable expansion by default (bash does), so `$INC` — a string
containing several embedded `-I path` pairs — collapsed into ONE argv
element that no `-I` flag parser recognizes as multiple paths; effectively
none of `$INC`'s paths reached the search list, only whatever separate,
individually-quoted `-I "$SINGLE_PATH"` flags followed it. This is the same
zsh trap already documented in
[[session-lexical-sub-escape-fix]]/CLAUDE.md conventions (`${=INC}` or
`bash -c` wrapping fixes it). **This specific combined-`$INC` pattern is
NOT what the original "Evidence" section above used** (that one passes each
`-I $VAR` separately, one single-path variable per flag, which is immune to
this particular trap) — so this finding does not retroactively explain the
original report, but it DOES mean: before trusting any *future* "could not
find module" observation in this codebase, first check whether the
reproducing command used a single shell variable holding multiple `-I
path` pairs expanded unquoted, and if so, re-verify via `bash -c` before
concluding it is a real interpreter bug. Both of these session's Cro-suite
"module resolution" observations (`http2-request-parser`,
`http2-request-serializer`, `http2-response-serializer`,
`http-response-parser`, `http-session-inmemory`, `http-session-persistent`
all transiently showing "Could not find X in module repositories" earlier
in this session) were this exact zsh artifact — re-verified via `bash -c`
immediately afterward, all six now show real, different, reproducible
`not ok` subtest failures instead (module loads fine) — recorded separately
where relevant, not further evidence for this ticket.

**Net effect: no confirmed live reproduction of the *original* per-`-I`-flag
pattern exists as of this update.** The next session that picks this up
should first attempt the original "Evidence" section's exact commands
(separate `-I $VAR` flags, always via `bash -c` or a `.sh` script to rule
out shell quoting entirely) several dozen times; if it still cannot
reproduce, consider closing this ticket as unreproducible/stale rather than
letting it block on a phantom.
