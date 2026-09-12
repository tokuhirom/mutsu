# Scratch interpreters stop rebuilding the process environment

A grammar-with-actions parse builds one lightweight *scratch* interpreter per
subrule-with-arguments call, per embedded code block and per token-method
dispatch — 3,109 of them on a 60-row `benchmarks/bench-yaml-parse.raku`
document. Round 10 of [#7576](https://github.com/tokuhirom/mutsu/issues/7576)
already stopped each of those from rebuilding the ~450-`ClassDef` built-in
registry. A callgrind profile of the post-round-11 binary showed
`Interpreter::new` still owning **916 M of the run's 6.09 Bn instructions
(15.1%)**, and the two things it was spending them on were both *process
constants* being recomputed 3,109 times.

## The bundled-battery scan ran once per scratch interpreter

`Interpreter::new` called `resolve_bundled_lib_paths()` unconditionally. That
is a `current_exe()` probe, a `read_dir` of the bundle base, and then a `join`
plus an `is_dir` stat for every distribution in it — about 40 entries. Per
parse that came to 3,109 directory scans: 124,360 `DirEntry::path`s, 133,687
`is_dir` stats, 149,232 `Path::join`s and 124,360 `Path::display().to_string()`
conversions, for **6.7% of the whole program's instructions**. Like round 11's
proto-variant scan, it never appeared under its own name in a self-cost profile
— the cost sat in `malloc`, `memcpy`, `Utf8Chunks::next` and
`CStr::from_bytes_with_nul`, and only caller attribution
(`callgrind_annotate --tree=calling`) put it back where it belonged.

The answer depends only on `MUTSU_BUNDLE_DIR` and the running executable's
location, neither of which can change under a running interpreter, so
`Interpreter::bundled_lib_paths_shared()` now memoizes it process-wide and
hands out a shared `Arc`. The memo is keyed on the `MUTSU_BUNDLE_DIR` value it
was taken under, so a process that changes the variable still re-scans; a
bundle directory whose *contents* change mid-process is not picked up, which is
what `-I` / `MUTSULIB` / `use lib` are for.

## …and so did the whole IO environment

`Interpreter::new` also called `init_io_environment()` unconditionally: four
`create_handle` calls for `$*OUT`/`$*ERR`/`$*IN`/`$*ARGFILES`, a
`make_io_spec_instance`, a `current_dir()` syscall, four `IO::Path` instances
for `$*CWD`/`$*TMPDIR`/`$*HOME`/`$*EXECUTABLE`, and eighteen env inserts —
**293 M instructions, 4.8% of the run**.

Every one of the ten scratch construction sites spells
`Interpreter { env: <the caller's env>, .. }`, so all of that seeded env is
dropped unread before the scratch executes anything. The seeding is now behind
the same `is_building_scratch()` guard the built-in registry already used.

That leaves the handle table. A handle op resolves the id carried by an
`IO::Handle` value in whichever table the *running* interpreter owns, and the
env a scratch is handed is the caller's — so its handle ids are the caller's
too. They agreed only because every fresh table was seeded with the same four
handles in the same order, putting them on ids 1-4 on both sides; nothing made
an id past those four agree. (No case was found where a scratch actually
performed a handle op that missed — code blocks reach their handles through the
caller's own VM frame — so this was a coincidence holding rather than a bug
biting.) Scratch interpreters are now built through
`new_regex_scratch_sharing_io()`, which shares the caller's
`Arc<RwLock<IoHandleTable>>` outright, so the ids mean the same thing on both
sides however many handles are open. Sharing is sound within a thread, which is
the only place a scratch is ever built; the lock discipline is unchanged.
`t/io/code-block-io-handle-table.t` pins the behaviour, driving handles whose
ids are deliberately past the four standard streams.

## Effect

On a 60-row variant of `benchmarks/bench-yaml-parse.raku`, measured with
`valgrind --tool=callgrind`
(deterministic and load-independent, per the method note rounds 10 and 11 added
to the ticket):

| | instructions | `Interpreter::new` inclusive |
| --- | ---: | ---: |
| before | 6,089,938,867 | 916,259,748 (15.05%) |
| after | 5,170,738,311 | 174,830,509 (3.38%) |
| | **-15.1%** | **-81%** |

`read_dir` calls over the whole parse go from 3,109 to 1. What is left in
`Interpreter::new` is the `Interpreter` struct's own construction — 369,981
allocations for its default-initialized fields — which is a different problem
from recomputing a process constant.

The next site on this ticket is now `parse_regex_uncached`: 597 M instructions
(11.6% of the reduced total) for 1,438 top-level parses, where a `rust-gdb`
hit-count sweep over the whole parse counts **10,958 parses of only 255 distinct
patterns**. The recursive sub-pattern entry point (`parse_regex_with_mode`, used
at ~15 sites inside the parser for groups, alternations, separators and scoped
patterns) has no cache at all, unlike `parse_regex`.
