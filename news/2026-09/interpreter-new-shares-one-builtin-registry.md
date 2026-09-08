# `Interpreter::new` shares one built-in registry, and drains its own garbage

[#7572](https://github.com/tokuhirom/mutsu/issues/7572) recorded two properties
of `Interpreter::new()`, measured while building ADR-0065 S2: **9.17 ms per
construction** and **+7.2 KiB retained per construction, linear** (debug build,
4000 construct-and-drop cycles). The issue's own guess was that the retention
lived somewhere in `runtime_init.rs` — "a `Box::leak`, a push into a
process-global registry, or a `Gc` allocation that outlives the drop" — and that
the `%*ENV` sweep was the wall-clock suspect. Both guesses were wrong, and the
two properties turned out to have nothing to do with each other.

Re-measured on the same probe, now in **release**: **1.17 ms → 0.26 ms** per
construction (4.5×), and the retention is gone. It used to grow in a straight
line; it now flattens:

| release, constructions | resident growth, before | after |
| --- | --- | --- |
| 1000 | +1.9 MB | +1.2 MB |
| 4000 | +4.9 MB (0.99 KiB/call, linear) | +2.2 MB |
| 16000 | (~16 MB, extrapolated) | +2.3 MB |

The debug build the issue measured tells the same story: 9.17 ms → 1.70 ms, and
+7.2 KiB/construction → +1.1 MB *total* over 4000.

## The retention was never in `Interpreter::new`

One measurement settled it:

| release, 4000 construct-and-drop cycles | KiB retained per construction |
| --- | --- |
| `MUTSU_GC` unset (on, the default) | 1.44 |
| `MUTSU_GC=off` | 0.08 |

Nothing in the constructor leaks. What accumulates is the GC's **cycle-candidate
buffer**: `Gc::drop` buffers a possible cycle root, and the buffer is drained by
`gc_safepoint`, which only the VM's dispatch loops and its call/await/join
boundaries emit. A loop that constructs an interpreter, drops it, and *never runs
any bytecode* therefore never reaches a single safepoint. Every dropped
interpreter's dead nodes were buffered and nothing ever came to collect them.

This also explains the line in the issue that reads like a dead end — "`MUTSU_GC=on`
changes nothing (9.26 ms, 7.31 KiB/call), so this is not a GC cycle waiting for a
collector that never runs in that loop". `MUTSU_GC=on` is the *default* (ADR-0003
§5), so that probe changed nothing about the configuration. It is exactly a
collector that never runs in that loop; `off` is the direction that shows it.

So `Interpreter::new` now emits a safepoint of its own, a new
`SafepointKind::Construct`. A top-level construction is a proper re-entry
boundary — no borrow, no lock, no `gc_contents_mut` is held — and for an embedder
driving mutsu per request it may be the only boundary it ever reaches. Scratch
interpreters (`new_regex_scratch`) are excluded: those are built from *inside*
regex/grammar evaluation, which is not a boundary.

## The wall clock was the registry, not `%*ENV`

A callgrind profile of 30 construct-and-drop cycles, 245.5M instructions total:

| | Ir | share |
| --- | --- | --- |
| `Interpreter::new` | 209.2M | 85% |
| ⤷ `build_builtin_registry` | 146.2M | 60% |
| ⤷ `os_env_hash` (the `%*ENV` sweep) | ~31.5M | 13% |
| `drop_glue::<Interpreter>` | 34.4M | 14% |
| ⤷ dropping that same registry | 29.7M | 12% |

Building ~450 `ClassDef`s and then throwing them away again was 72% of the cycle.
The `%*ENV` sweep the issue suspected was 13%.

That registry is identical in every interpreter in the process, and it has been
copy-on-write for as long as `clone_for_thread` has existed: every mutation goes
through `RegistryWriteGuard`, whose `deref_mut` is an `Arc::make_mut`. So it is
now built **once per process** and handed out as a shared `Arc`
(`Interpreter::shared_builtin_registry`). Sharing a process-wide template is the
same share `clone_for_thread` already performs between threads, one level up.

### The write that made the share worthless

Sharing alone only bought 41%, and the profile said why: 33% of what was left had
moved into `RegistryWriteGuard::deref_mut`. Something was *writing* the registry
during construction, and the first write forks a private deep copy — so every
construction was still paying for a whole registry, now as a clone instead of a
build.

The writers were the five `init_*_enum` calls. Each one built the base-tier
`Value`s for a built-in enum (`Order`, `Endian`, `ProtocolFamily`, `Signal`,
`SeekType`) *and* recorded the enum type itself through `self.registry_mut()`.
Those five entries are process constants, so they belong in the template: the
variant lists are now shared between `init_*_enum` (values) and a new
`seed_builtin_enum_types` (types), which `build_builtin_registry` calls. The
`init_*_enum` functions no longer take `&mut self` at all, which is the honest
statement that they no longer touch the registry.

With the write gone, 1.17 ms → 0.28 ms.

### `val()` stopped copying every string it was about to reject

What that leaves at the top of the profile is the `%*ENV` sweep after all — 55%
of the remainder, of which three quarters is `builtin_val`, because `%*ENV`
values are allomorphs and every one of ~40 environment variables goes through the
full Raku numeric-string parser.

Two allocations per attempt were pure waste. `normalize_minus` copied the whole
string just to leave it unchanged when no U+2212 MINUS SIGN was present, and
`strip_underscores` built a fresh `String` for *every* candidate the parser tried
— including the ones it rejected on the next line, and almost nothing reaching it
has an underscore at all. `strip_underscores` alone was 18% of a construction.
Both now return `Cow` and borrow when there is nothing to change, which is a win
for every `val()` in the language, not only this sweep: 0.28 ms → 0.26 ms here.

## What a one-shot `mutsu script.raku` pays

The template `Arc` is held by a `OnceLock`, so a single-interpreter process's
registry is no longer uniquely owned and its first registry write pays one
`Arc::make_mut` deep clone — measured at ~1.6M Ir, under a third of the ~5.3M Ir
the build it replaces still costs once. Against that it drops five `registry_mut`
acquisitions from startup and gets the `val()` improvement. Programs that declare
nothing never write the registry and pay nothing at all.

## Pins

`tests/long_lived_parse.rs`, the file the issue points its repro at, grew two:

- `repeated_interpreter_construction_does_not_grow_without_bound` — the retention
  gate. Its bound is deliberately **independent of the iteration count**, because
  that is now the property: the candidate buffer is capped by the collector's
  size threshold, so a 4000-construction run must grow like a 1000-construction
  one. The old behaviour blew through it at `MUTSU_S0_ITERATIONS=4000`, where it
  retained 28.9 MB.
- `a_declaration_in_one_interpreter_is_invisible_to_another` — the correctness
  gate on the share. Two interpreters alive at once really do hold the same
  template `Arc`; a class declared in one must not be visible in the other, which
  is the copy-on-write fork actually happening.

Wall-clock figures here are local probe runs (this container, release unless
stated), not bench-CI rows; the instruction counts are callgrind, which is
load-independent.
