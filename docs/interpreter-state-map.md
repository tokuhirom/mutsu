# `Interpreter` state map

Phase 2 of #10779: which parts of the source touch each of `struct Interpreter`'s fields, and
which subsystems those fields fall into. This is analysis only; it changes no code. It is the
input to the phase-3 ADR that decides the subsystem boundaries. Measured 2026-10-03 on
`main` at `b3fd61e43`.

Regenerate the full tables (per-field file counts, co-occurrence clusters, every field's
subsystem) with:

```sh
scripts/interp-field-matrix.py                  # markdown report
scripts/interp-field-matrix.py --json OUT.json  # plus the raw field x file matrix
```

## Method

`scripts/interp-field-matrix.py` reads the 438 fields of `pub struct Interpreter`
(`src/runtime/mod.rs`) and counts, per source file, the accesses to each one. An access is
either `.<field>` (not a method call) in a file that implements or handles `Interpreter`, or a
call to one of the 352 *accessors*: short `Interpreter` methods (at most 6 lines) that touch
exactly one field, such as `registry()`, `env_mut()` and `take_pending_call_arg_sources()`.
Without the accessors, `registry` (reached only through `registry()`/`registry_mut()`) would
look unused.

The count is textual, so read it with these limits in mind:

- 19 field names (`env`, `stack`, `locals`, `current_package`, ...) are also field names of
  another struct (`LazyList::env`, for example). Their counts include some accesses to that
  other struct. The report marks them with `*`.
- An access through a longer helper method is attributed to the helper's file, not to the
  helper's callers.
- Two files walk the whole state rather than use part of it. `runtime/runtime_thread.rs`
  (`clone_for_thread`) touches 133 fields, and `runtime/gc_roots.rs` touches 63. They are left
  out of the clustering and the coupling counts below.

## How widely the fields are used

| files touching the field | fields |
|---|---|
| 1 | 58 |
| 2-3 | 174 |
| 4-9 | 144 |
| 10-29 | 50 |
| 30+ | 12 |

Only 12 fields are touched from 30 or more files: `env` (334 files), `registry` (184), `stack`
(115), `locals` (91), `current_package` (74), `routine_stack` (65), `current_package_sym` (52),
`instance_type_metadata` (40), `pending_call_arg_sources` (36), `current_unit` (32),
`pending_rw_writeback_sources` (32) and `cur_source_line` (30). 232 of the 438 fields (53%)
are touched from at most 3 files. The god object is mostly many small pieces of state with a
narrow reach, around a small, widely shared core.

## Co-occurrence clustering does not find the subsystems

The script also clusters the fields by which files touch them (average-linkage Jaccard, cut at
0.25). This gives 90 groups, but they follow file layout more than meaning. For example, the
accessor file `runtime/accessors_state.rs` links `hll_syms`, the escaping-`our` bookkeeping
and `func_multi_resolve_cache` into one cluster, only because their accessors sit next to each
other. The subsystems below were therefore assigned by hand, from each field's name, type and
doc comment. They are kept as ordered regex rules (`SUBSYSTEMS` in the script), so that a new
field which matches no rule shows up as *unclassified*. All 438 fields are classified today.

## Proposed subsystems

| subsystem | what it holds | fields | files touching it | fields in 30+ files |
|---|---|---|---|---|
| frame | VM frame and execution core | 34 | 402 | `env`, `stack`, `locals`, `routine_stack`, `current_unit`, `cur_source_line` |
| handoff | Call-site side channels: implicit arguments between caller, binder and VM | 39 | 123 | `pending_call_arg_sources`, `pending_rw_writeback_sources` |
| topic | Topic, given/when, for/loop bookkeeping | 22 | 45 | - |
| control | Control flow, exceptions, phasers, program exit | 26 | 63 | - |
| io | Output, IO handles, process environment, TAP | 18 | 72 | - |
| module | Module loading, compunits, import/export, pragmas | 71 | 112 | - |
| types | Type/package registry and declarations (MOP) | 43 | 266 | `registry`, `current_package`, `current_package_sym`, `instance_type_metadata` |
| lexicals | Package/unit/state/`our` variable storage outside frames | 40 | 92 | - |
| threads | Cross-thread shared variables and locks | 13 | 31 | - |
| dispatch | Dispatch state: multi/method/wrap/samewith stacks, dispatch flags | 30 | 61 | - |
| caches | Resolution and compile caches (derived and rebuildable) | 51 | 44 | - |
| async | Supply/react/gather/lazy-pull state | 23 | 39 | - |
| regex | Regex, grammar and slang state | 10 | 22 | - |
| eval | EVAL/REPL/MAIN and compile-time capture analysis | 14 | 20 | - |
| guards | Recursion/cycle guards for `.raku`/`.gist` | 4 | 3 | - |

Files that touch the interpreter's state, by how many subsystems they touch: 1 subsystem: 217,
2: 135, 3: 68, 4: 66, 5 or more: 70.

Coupling, counted as the number of files that touch both subsystems (excluding the two
whole-state files):

| | frame | handoff | topic | control | io | module | types | lexicals | threads | dispatch | caches | async | regex | eval | guards |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| frame | **402** | 106 | 43 | 57 | 49 | 93 | 180 | 85 | 29 | 52 | 32 | 26 | 14 | 17 | 2 |
| handoff | 106 | **123** | 20 | 27 | 21 | 34 | 71 | 40 | 16 | 35 | 15 | 8 | 8 | 9 | 2 |
| topic | 43 | 20 | **45** | 21 | 9 | 14 | 19 | 23 | 9 | 3 | 3 | 12 | 1 | 3 | 0 |
| control | 57 | 27 | 21 | **63** | 21 | 26 | 35 | 25 | 6 | 12 | 6 | 13 | 2 | 10 | 0 |
| io | 49 | 21 | 9 | 21 | **72** | 28 | 34 | 14 | 2 | 12 | 3 | 9 | 3 | 11 | 1 |
| module | 93 | 34 | 14 | 26 | 28 | **112** | 86 | 39 | 6 | 23 | 18 | 8 | 3 | 7 | 1 |
| types | 180 | 71 | 19 | 35 | 34 | 86 | **266** | 60 | 13 | 44 | 37 | 12 | 12 | 13 | 2 |
| lexicals | 85 | 40 | 23 | 25 | 14 | 39 | 60 | **92** | 17 | 12 | 12 | 11 | 3 | 5 | 0 |
| threads | 29 | 16 | 9 | 6 | 2 | 6 | 13 | 17 | **31** | 4 | 4 | 3 | 2 | 2 | 0 |
| dispatch | 52 | 35 | 3 | 12 | 12 | 23 | 44 | 12 | 4 | **61** | 17 | 5 | 2 | 5 | 2 |
| caches | 32 | 15 | 3 | 6 | 3 | 18 | 37 | 12 | 4 | 17 | **44** | 3 | 3 | 1 | 1 |
| async | 26 | 8 | 12 | 13 | 9 | 8 | 12 | 11 | 3 | 5 | 3 | **39** | 2 | 1 | 1 |
| regex | 14 | 8 | 1 | 2 | 3 | 3 | 12 | 3 | 2 | 2 | 3 | 2 | **22** | 2 | 1 |
| eval | 17 | 9 | 3 | 10 | 11 | 7 | 13 | 5 | 2 | 5 | 1 | 1 | 2 | **20** | 0 |
| guards | 2 | 2 | 0 | 0 | 1 | 1 | 2 | 0 | 0 | 2 | 1 | 1 | 1 | 0 | **3** |

## Findings for the phase-3 ADR

1. **`frame` is the VM itself, not a subsystem to extract.** 402 files touch it, and `env`,
   `stack` and `locals` are what every opcode reads. Extraction should run the other way: move
   the other subsystems out, and leave `Interpreter` as the frame core plus one field per
   subsystem.

2. **`handoff` is a design smell, not a subsystem.** Its 39 fields (`pending_call_arg_sources`,
   `pending_rw_writeback_*`, `pending_caller_var_writeback`, `trait_mod_*_writeback*`,
   `in_lvalue_assignment`, `static_call_args`, ...) are set just before a call and taken by the
   callee (`set_pending_call_arg_sources` / `take_pending_call_arg_sources`). They are
   arguments passed through shared mutable state. 106 of the 123 files that touch them also
   touch `frame`. Putting them in a struct would only move the smell. The fix is to turn each
   one into an explicit parameter or return value. This is also a correctness gain: a flag that
   is set and then not taken (for example on an error path) leaks into the next call.

3. **Most subsystems are well bounded.** Leaving out `frame`, `types` and `handoff`, the largest
   overlap between two subsystems is 39 files (`module` with `lexicals`). Several are nearly
   self-contained: `guards` (3 files), `eval` (20), `regex` (22), `threads` (31), `async` (39)
   and `caches` (44). These are the cheap first extractions, and each gives a type with its own
   API.

4. **`caches` is the easiest large win.** It has 51 fields and 44 files, and holds derived
   state only: resolution memos, method/multi caches, call-lane tables and compile caches.
   About ten of them are `GenCache`s, and most of the others are invalidated by a generation
   counter (`fn_resolve_gen`, `method_cache_generation`, ...). They would fit in one
   `ResolutionCaches` type with a single invalidation entry point. Because the state is
   derived, a spawned thread could also start with empty caches instead of cloning them.

5. **`types` is pervasive because of its read side.** `registry` is already its own type
   behind `registry()`/`registry_mut()` guards (184 files). What spreads `types` is
   `current_package`/`current_package_sym` (74 and 52 files) and `instance_type_metadata` (40).
   `current_package` arguably belongs to the frame, since it is the lexical package of the
   running code.

6. **The whole-state walks are where extraction pays first.** `clone_for_thread` names 133
   fields one by one, and the GC root scan names 63. With subsystem types, each type would
   implement its own thread-clone policy and root tracing. A field added to a subsystem then
   could not be forgotten by either walk. Today, forgetting it is a silent bug: the field is
   not inherited by a thread, or not rooted for the GC.

7. **There are precedents.** `OutputSink`, `TapState`, `IoHandleTable`, `Registry`,
   `RoutineStack`, `SharedStore`, `OnceStore`, `CurRepoState`, `MarkContextState` and
   `ReplCompilerState` have already been extracted, each behind accessors. The phase-3 work is
   to apply the same pattern to the subsystems above.

A suggested order: `guards`, `caches`, `regex`, `eval`, `threads` and `async`, which are small
and bounded. Then `io`, `control`, `topic`, `dispatch` and `lexicals`. Then `module`, which is
the largest bounded one. The `handoff` fields are removed one by one throughout, as explicit
parameters, not as a subsystem.
