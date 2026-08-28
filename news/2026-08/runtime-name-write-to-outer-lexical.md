# A write through a run-time-resolved variable name now reaches its outer lexical

`$::($name) = v`, `::('$x') = v`, and an assignment inside an `EVAL`'d snippet
all name their target at **run time**. Such a write reached the outer lexical at
mainline and inside a bare block, but was silently lost — no error, the variable
simply kept its old value — as soon as it happened inside a closure that is
*invoked*, or inside a routine.

```raku
my $z = 1; $::('z') = 11;                            # 11   OK
my $z = 1; { $::('z') = 22 };                        # 22   OK   (bare block)
my $z = 1; my $c = { $::('z') = 33 }; $c();          #  1   WRONG (raku: 33)
my $z = 1; sub w(&f) { f() }; w({ $::('z') = 44 });  #  1   WRONG (raku: 44)
my $z = 1; sub w2() { $::('z') = 55 }; w2();         #  1   WRONG (raku: 55)
```

`EVAL` showed the identical shape (`my $a; my $c = { EVAL q|$a = 32| }; $c()`
left `$a` at `Any`), which is what made it look like one bug. It was not: the
two constructs failed for three genuinely different reasons, and all three had
to be fixed.

## Root cause 1 — the frame-exit writeback is entirely compile-time

Both `call_compiled_closure_with_topic` and `call_compiled_function_named_inner`
propagate a callee's mutations to the caller through a filter keyed on
compile-time knowledge: `free_var_syms`, `free_var_writes`, `captured_names`,
and "the caller env already holds this key". A runtime-resolved target satisfies
none of them — the compiler never saw the name — so the write died with the
frame.

`OpCode::SymbolicDerefStore` is not even in `CompiledCode::has_env_writes`, so
for a closure whose body was *only* a symbolic store the whole writeback scan was
skipped as "this frame changed nothing"; adding any call to the same body
(`{ $::('z') = 33; 1.Str }`) made it work, which is what first localized this.
(That flag is left alone: the escape hatch below runs *outside* the
`needs_caller_writeback` gate, so widening the flag — and with it the broad
env scan — would buy nothing and only add regression surface.)

The fix keeps the precise filters and adds one narrow escape hatch. A by-name
write whose target is not a local of the writing frame is recorded on
`pending_runtime_name_writes` (a new list, deliberately separate from the
long-standing `pending_caller_var_writeback` — see below), and each frame exit
copies those names' live values into the caller's env
(`propagate_pending_caller_writes`). The existing retain-on-miss drain then
refreshes the slot in whichever frame actually declares the lexical, however
many frames up that is. Programs that never make a runtime-name write keep an
empty list and pay nothing.

## Root cause 2 — an EVAL'd unit did not record its free-variable writes

`eval_block_value_recording_writes` already existed for exactly this problem, but
only `where` clauses used it. The *snippet's own* compiler knows perfectly well
that `$a` in `EVAL '$a = 32'` is a free variable it writes, so EVAL'd units now
record that set too, and it feeds the same mechanism as root cause 1.

## Root cause 3 — `eval_pre_lexicals` scanned only the innermost env tier

`parse_and_eval_with_operators` snapshots the plain-user-lexical keys present
before the snippet runs so it can drop the ones the snippet's own `my`
introduced. That snapshot used `Env::keys`, which exposes **only the innermost
tier's overlay**. Inside a closure or a routine the frame env is a scoped child
and the caller's lexicals live in a parent tier, so every one of them looked
brand-new: `EVAL '$a = 32'`, whose write lands in the overlay, was classified as
an EVAL-local declaration and deleted on the way out.

Worse, removing a name from a scoped env leaves a **tombstone**, which hid the
caller's binding outright — a second `EVAL 'say $a'` in the same block died with
"Variable '$a' is not declared". Switching to `visible_keys_where` (the identical
fix already applied to the `&`-code-var shadow snapshot in `eval_eval_string`)
fixes both. A snippet's genuine `my` is still dropped, and a `my` that shadows a
caller name is still restored by the separate `eval_shadowed` path, so
`EVAL 'my $a = 999'` still cannot clobber a caller's `$a`.

## Two traps found by CI-grade evidence, worth remembering

**Do not replay a system name across a frame boundary.** The first version
propagated everything on `pending_caller_var_writeback`, which included
`&?BLOCK` (recorded from an EVAL'd unit's `free_var_writes`). Carried upward, it
made the real `Test::throws-like`'s `subtest` block run against the *previous*
subtest's `&?BLOCK`, so its `CATCH` saw `Any` instead of the exception: two
consecutive `throws-like { EVAL … }` calls left the second reporting "right
exception type" as failed. Only plain user lexicals are eligible now.

**Do not reuse a broadly-fed list for a narrow purpose.** Even filtered to plain
lexicals, propagating *all* of `pending_caller_var_writeback` was too blunt — that
list is also fed by `is rw` writeback misses, Proxy STOREs, `$CALLER::x` writes
and the shared-var lane. Replaying those into every intervening caller env broke
a `given $in { when IO::Handle {…} }` dispatch in the bundled Text::CSV
(`Type check failed in assignment to $io-in`), which `make test` and a full
targeted roast sweep both missed and only `scripts/battery-testsuite.sh` caught.
Hence the dedicated `pending_runtime_name_writes` list.

## Result

`t/runtime-name-write-to-outer-lexical.t` pins all six positions (mainline, bare
block, invoked closure, closure passed to a sub, a named sub's own body, two
frames deep, and a block a Raku-level routine calls) for `$::(…)`, `::('$x')` and
`EVAL`, plus the sigilless-`rw`-through-`EVAL` shape and the four must-not-regress
`my`-stays-EVAL-scoped cases. All 28 assertions pass under real `raku` as well as
under mutsu.

Three roast files went from failing under `MUTSU_REAL_TEST=1` to passing under
**both** providers:

| file | before (real) | after (real) | native |
| --- | --- | --- | --- |
| `S02-lexical-conventions/comments.t` | 1 failure (#41) | **PASS** | still PASS |
| `S06-signature/sigilless.t` | 1 failure (#5) | **PASS** | still PASS |
| `S02-names/symbolic-deref.t` | 2 failures (#3, #14) | **PASS** | still PASS |

`sigilless.t` is worth calling out: its failing assertion is an `is rw` writeback
performed by a routine the EVAL'd snippet *calls*, one closure-invocation deep —
a third spelling of the same "the write cannot find its frame" problem, fixed by
the same mechanism.
