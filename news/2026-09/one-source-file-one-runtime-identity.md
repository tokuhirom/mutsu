# One source file, one runtime identity

mutsu named a compilation unit twice over, and the two names disagreed. The guard
that stamps every compiled chunk (`CompiledCode::source_file`, ADR-0106 Slice 0)
got the program path run through `fs::canonicalize`; the env's `?FILE` — which is
what a `RoutineFrame`, `Code.file`, `CallFrame.file` and a backtrace report — got
the path as spelled on the command line. Run a script from its own directory and
one file was both `/home/user/mutsu/tmp/prof.raku` and `prof.raku`:

```console
$ cd tmp && MUTSU_PROFILE=1 ../target/debug/mutsu prof-demo.raku
profile: line /home/user/mutsu/tmp/prof-demo.raku:5 hits=600000     # chunk identity
profile: callsite prof-demo.raku:12 -> GLOBAL::slow calls=30        # frame identity
```

Two rows describing one file that cannot be joined. The profiler was the first
consumer to read both identities into one table and therefore the first that
could not ignore it; it worked around it at report time in `src/profile/paths.rs`,
and [#8719](https://github.com/tokuhirom/mutsu/issues/8719) tracked settling it at
the source.

## What decided it

Which spelling wins is a Raku-visible question, so the answer came from measuring
rakudo rather than from picking the tidier string — and the measurement produced a
surprise worth writing down. **rakudo has two spellings too**: `$?FILE` is
absolute, while `Code.file` and `CallFrame.file` are the path as typed. So
collapsing mutsu's two onto one string would have been a divergence whichever one
it picked.

The second measurement is what made the problem tractable. rakudo *absolutifies*;
it does not canonicalize. Invoke `raku ./tmp/sub/../x.raku` and `$?FILE` comes back
as `<cwd>/./tmp/sub/../x.raku` — dots intact, symlinks unresolved. `$?FILE` is
`$*CWD` joined with the path as spelled, a total string function over a value
mutsu already has.

That is the whole fix: the **as-invoked path is the one runtime identity**, and
`$?FILE` is *derived* from it rather than being a second identity beside it. The
mainline now publishes the same string to the unit stamp and to the env, which is
what a `use`d module and an `EVAL` had been doing all along — ADR-0106 §7 had
already ruled on the `EVAL` case, and the argument had simply never been carried to
the mainline. `-e`, `-`, `<unknown>` and `<repl>` are pseudo-names with nothing to
resolve them against, so they are left alone; rakudo reports a bare `-e` for
`$?FILE` too. The decision is [ADR-0107](../../docs/adr/0107-compilation-unit-runtime-identity.md).

## What changed

`$?FILE` came out *more* rakudo-accurate, not less. It used to be canonicalized, so
a path with `.`/`..` components or a symlink in it reported a string the user never
typed; it now matches rakudo exactly in all three cases measured. The
`fs::canonicalize` syscall on every program start is gone with it.

`src/profile/paths.rs` is deleted, along with its callers in `src/profile/counts.rs`
and `src/profile/aggregate.rs` — and with them both of the report's second folds.
Those existed only because reconciling a file could collapse two keys onto one and
the rows underneath had to be summed again; with one identity, `totals` and `folded`
are already final, so each drain is a plain collect instead of a re-hash of every
row in the profile.

`--dump-bytecode` stops disagreeing with a real run of the same script: the flag's
own path always published the as-invoked `program_name`, and only `run()` published a
canonicalized one.

`t/tooling/profiler-unit-file-identity.t` pins the result — both Raku-visible spellings, the
`.`/`..` preservation, the `-e` pseudo-name, and the profile document's single file
row. Reverting the runtime change fails three of its fourteen assertions, including
the one that catches the split file row.

## What is left

A frame's `file` is the *dynamically scoped* `?FILE`, which still names the mainline
while a `use`d module's routine runs, so a call made inside a module is filed under
the script's path with the module's line numbers. That is a separate divergence with
a much larger blast radius — fixing it changes backtrace text — so it keeps its
profiler-side reconciliation in `ProfileAggregate::resolve_caller_files` and is now
tracked as [#8743](https://github.com/tokuhirom/mutsu/issues/8743).
