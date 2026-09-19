# ADR-0107: A compilation unit's runtime identity is the path as invoked; `$?FILE` is its absolutified form

- Status: Accepted (implemented)
- Date: 2026-09-18
- Closes: [#8719](https://github.com/tokuhirom/mutsu/issues/8719)
- Related: [ADR-0106](0106-raku-level-profiler.md) §5 Slice 0 (the chunk-level unit stamp this decides the value of)

## 1. Context

mutsu names a compilation unit in two places, and until now the mainline put a different string in each:

| Where | What it feeds | Mainline value before this ADR |
| --- | --- | --- |
| `unit_source_file` (the guard around every compile) | `CompiledCode::source_file`, `location_at`, `--dump-bytecode` annotations, the profiler's line rows | `fs::canonicalize(program_path)` |
| the env's `?FILE` | `RoutineFrame { file }`, `Code.file`, `CallFrame.file`, backtrace text, `FunctionData::source_file`, the profiler's routine and callsite rows | `program_path` as spelled on the command line |

Run a script from its own directory and one file was both `/home/user/mutsu/tmp/prof.raku` and `prof.raku`. Anything keyed on `(file, line)` and filled from both sources — a profile, a coverage report, a language-server index — split one file's data in two, and a caller row could not be matched to the line row it belonged to.

This was the mainline alone. A `use`d module (`run_modules.rs`) and an `EVAL` (`builtins_eval_misc.rs`) already published **one** string to both, and ADR-0106 §7 had already ruled on the `EVAL` case explicitly, on the grounds that "a second identity would have made `location_at` disagree with the frame beside it". The same argument had simply never been applied to the mainline.

ADR-0106 Slice 2 shipped a report-time workaround, `src/profile/paths.rs`: canonicalize a location's file when the name resolves to a real file, leave it alone when it does not (so `EVAL_1` and `<unknown>` survived), memoized per interned symbol. It made the profile's tables joinable without touching runtime semantics, and it was explicitly a patch for one consumer.

## 2. What rakudo does — measured, not assumed

The deciding question is what rakudo reports, because `$?FILE`, `Code.file`, `CallFrame.file` and backtrace text are all Raku-visible. Measured against the rakudo in this container:

```
$ cd /home/user/mutsu/tmp && raku 8719-file.raku
$?FILE       /home/user/mutsu/tmp/8719-file.raku      # $*CWD-joined
Code.file    8719-file.raku                           # as spelled

$ cd /home/user/mutsu && raku ./tmp/sub/../8719-file.raku
$?FILE       /home/user/mutsu/./tmp/sub/../8719-file.raku
Code.file    ./tmp/sub/../8719-file.raku

$ cd /home/user/mutsu && raku tmp/link-to-file.raku      # a symlink
$?FILE       /home/user/mutsu/tmp/link-to-file.raku      # the link, not its target
Code.file    tmp/link-to-file.raku

$ raku -e 'say $?FILE'
-e
```

Two facts fall out, and both matter:

1. **rakudo itself has two spellings** — an absolute `$?FILE` and an as-spelled `Code.file`. Collapsing them onto one string is therefore *not* the compatible answer; a mutsu that reported one everywhere would diverge from rakudo whichever one it picked.
2. **rakudo absolutifies; it does not canonicalize.** `.` and `..` components survive and a symlink is not resolved. `$?FILE` is exactly `$*CWD` joined with the path as spelled — a pure string operation on a value mutsu already has.

Fact 2 is what makes the divergence fixable without giving anything up: if `$?FILE` is *derived* from the as-invoked path by a total, syscall-free function, then the as-invoked path can be the single runtime identity and `$?FILE` costs nothing to keep rakudo-accurate.

## 3. Decision

**The as-invoked path is *the* runtime identity of a compilation unit.** The unit stamp and the env's `?FILE` publish the same string, so `CompiledCode::source_file` and a `RoutineFrame`'s `file` are the same interned symbol. This is what a `use`d module and an `EVAL` already did; the mainline now joins them.

**`$?FILE` is that identity absolutified, and is not an identity of its own.** It is `$*CWD`-joined when the unit's name is a relative path, and left alone when it is already absolute or is a *pseudo* name (`-e`, `-`, `<unknown>`, `<repl>`) — rakudo reports a bare `-e`, not `$*CWD/-e`. Absolutified, never canonicalized: folding `.`/`..` or resolving a symlink would be a measured divergence from rakudo (§2), would cost a syscall, and — the architectural point — would make `$?FILE` underivable from the identity, which is how two identities got here in the first place.

### Why not the other two candidates

- **Make everything canonical.** Changes `Code.file`, `CallFrame.file` and every backtrace to a path the user never typed, diverging from rakudo on all of them, and keeps the syscall. Five `t/` files pin the current text, correctly.
- **Make everything as-invoked, `$?FILE` included.** Loses `$?FILE`'s absoluteness, which rakudo has and which `t/modules/module-file-var-and-callframe.t` and `t/modules/compunit/eval-compunit-introspection.t` pin. It is also the less useful of the two: `$?FILE` is the one of the pair a program hands to `.IO`.

## 4. Consequences

- `src/profile/paths.rs` is deleted, with its callers in `src/profile/counts.rs` and `src/profile/aggregate.rs`. The report's two second folds go with it: `totals` and `folded` are already final tables, so each drain is a plain collect rather than a re-hash of every row.
- The `fs::canonicalize` on every program start is gone.
- `$?FILE` becomes *more* rakudo-accurate, not less: a path spelled with `.` or `..`, or through a symlink, now reports the way rakudo reports it.
- `--dump-bytecode` is now consistent with a real run. The flag's own path (`lib::dump_bytecode`) always published the as-invoked `program_name`, so its listing already disagreed with the stamp `run()` published for the very same script; both are the as-invoked path now.
- A consumer that wants a filesystem-canonical path still has to ask for one — that is `.IO.resolve`'s job, and deliberately not the runtime's.

`t/tooling/profiler-unit-file-identity.t` pins all of it: the two Raku-visible spellings, the `.`/`..` preservation, the `-e` pseudo-name, and the profile document's single file row.

## 5. What this does NOT settle (settled by #8743)

A frame's `file` used to be the **dynamically scoped** `?FILE`, which still named the mainline
while a `use`d module's routine ran — so a call made inside a module was filed under the script's
path with the module's line numbers. That was a different divergence with a much larger blast
radius (it changed backtrace text) than the one this ADR settles, and it kept its own
profiler-side reconciliation in `ProfileAggregate::resolve_caller_files`. Settled by
[#8743](https://github.com/tokuhirom/mutsu/issues/8743): every `RoutineFrame` push now resolves
its call-site `file` via `Interpreter::executing_source_file_sym` (an outward walk over the live
`routine_stack` for the nearest frame that names its own file), not `?FILE`, so a backtrace,
`CallFrame.file` and the profiler's counters all read the correct file with no reconciliation pass
left to run.
