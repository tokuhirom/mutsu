# `grep`'s element promotion is published in place, not by re-binding names

`@a.grep(...)` promotes each matched source slot to a shared `ContainerRef`
cell and hands the same cells to the result, so `for @a.grep(...) { $_++ }`
mutates through into `@a`. The promotion was *published* by building a
replacement `ArrayData` and calling `overwrite_array_bindings_by_identity` —
which walks the **current frame's `env`** looking for names that point at the
old one.

That route has two problems, and only the second is why it was touched.

- It only ever reached the aliases that happened to be lexically visible at the
  `.grep` call. A source array reachable some other way silently kept the
  unpromoted slots, and the replacement was discarded. It also needed a
  `pending_rw_writeback_sources` drain to stop the caller's local slot going
  stale behind the by-name write.
- It is **frame-dependent**, which is what blocks ADR-0058 step 3b. A deferred
  `grep` promotes at *pull* time, in a frame where the source's names are long
  gone, so the env walk finds nothing and the writeback disappears. §9.2
  recorded this as "deferring grep moves the promotion and its identity-keyed
  writeback to pull time, in a different frame"; the sharper statement is that
  the publication mechanism itself cannot survive the move.

The promotion is now published by mutating the source `Gc<ArrayData>` in place.
ADR-0013 §7's `GcBox`/`UnsafeCell` interior-mutability refinement made this
sound at the primitive, and CLAUDE.md records that it unblocks exactly this kind
of work. Writing through the `Gc` reaches every alias by construction, needs no
drain, and does not care which frame is running.

## What that surfaced, which is the point

Making the publication universal means a source that was previously skipped now
really does end up holding element cells — so every reader of that array has to
look *through* them. Two `Backtrace` readers did not:

```raku
my sub bar { die }();
CATCH { default {
    my $bt = .backtrace;
    say $bt.summary.chars;      # 151
    $bt.grep({ !.is-hidden });
    say $bt.summary.chars;      # 0
} }
```

`concise`/`summary`/`full` matched `ValueView::Instance` on each frame directly,
and `backtrace_methods`'s `frame_field` / `is_routine` / `is_setting` did the
same. A promoted frame matched none of them, so `.summary` came back empty and
`.nice` / `next-interesting-index` lost their filtering. The three accessors are
fixed at their one chokepoint — `frames_of` dereferences as it builds the list —
so a future reader inherits the fix instead of repeating the bug.

This is the decont-leak class ADR-0039's history warns about (a previous attempt
at decl-site cell-boxing regressed ~12 files through it). The measured blast
radius here is **two files**: `t/backtrace-frame.t` and
`t/backtrace-introspection.t`, 9 subtests between them. `make test` (3784 files
/ 39776 tests) and a full local `make roast` (1436 files / 218962 tests) are
otherwise unchanged.

Pinned by `t/grep-promotion-does-not-hide-frames.t` (6 rows, green under rakudo
too), which asserts both halves: a backtrace survives a `.grep` over itself, and
the writeback the promotion exists for still works while an `=` copy of the
result still decontainerizes.

## The second thing it surfaced: a stale writeback clobbering a captured topic

The bundled-library battery gate then caught `URI mutate.rakutest`, and it turned
out to be a **pre-existing, general bug the universal publication merely made
visible**. Minimal repro (rakudo prints the array unchanged; mutsu blanked out an
element):

```raku
my @src = 1, 2, 3;
sub bump(\x) { x = x + 0 }
for @src { bump($_) }              # parks `_` on the pending-writeback list

class Q {
    has Pair @!qf;
    method setup(@p) { @!qf = @p }
    method !value-for($k) {
        my $l = @!qf.grep({ .key eq $k }).map({
            my $v = .value;
            Proxy.new(FETCH => method () { $v },
                      STORE => method ($n) { die "read-only" });
        }).List;
        $l[0]
    }
    method AT-KEY($k) { self!value-for($k) }
}
```

`call_compiled_closure_in_unit` ends every closure call by rejoining an
rw-argument writeback to the cell the closure captured under that source name —
the mechanism behind `my $i = 0; sub inc($o is rw) { $o++ }; my $t = { inc($i) };
$t(); $t();` answering 2. Its trigger was the *process-wide* pending lists, with
no check that the closure has anything to do with the name.

`pending_caller_var_writeback` is retain-on-miss: an entry stays until some
frame's `code` owns a local slot for it, so a name no frame can ever own — the
implicit topic `_`, `$/`, `@_`, a package or enum constant — is retained for the
rest of the run. (`_` gets there from a sigilless parameter aliasing `$_`:
`exec_set_local_op_inner` records the alias chain's root as a writeback source,
and the drain then migrates it to the retain-on-miss list.) Every later closure
call therefore re-ran the rejoin for `_`, storing the CALLING frame's ambient
`$_` through whatever cell the closure's captured env held under that name.

A `.map`/`.grep` block's `$_` is the source element's own container, so a closure
created inside the block captures `_` bound to that element cell. `URI::Query`
builds exactly such a closure — the `Proxy` reader in `!value-for` — and calling
it from a method frame, whose ambient `$_` is undefined, wrote `Any` straight
into `@!query-form`'s element. The next `ASSIGN-KEY` then died with "No such
method 'key' for invocant of type 'Any'". Before this PR the promotion never
reached the attribute array, so the fabricated write landed in a transient grep
result and nobody saw it.

The fix gates that loop on `cc.capture_free_var_set()`: only a lexical this body
actually *reads* can be the source of a writeback into its own capture.
`data.env` cannot make that call — it is the whole captured enclosing env, not
the set of names the body uses — while `free_var_syms` is exactly that set, and
is already the gate the per-instance persistence loop right below uses. Pinned by
`t/closure-captured-topic-writeback.t` (green under rakudo; 2 of its 4 rows fail
without the gate).

## What is left of ADR-0058 step 3b

The grep mode on `SeqSource::MapGrep` (its pull arm runs `eval_map_over_items`
unconditionally) and the two entry points. The listop `grep({ $_ = 5 }, @a)`
form separately does not write back at all — `builtin_grep` has no `source_var`
path — which is a pre-existing bug measured here and unrelated to deferral.
