# A read-only closure over a multi-parameter loop variable snapshots the element

A `for` loop parameter binds the element's container, so a closure created in
the body reads *through* it — a later write to the element is visible. That
works for every single-parameter form. A **multi-parameter** loop loses the read
direction, while keeping the write direction:

```
$ raku  -e 'my @a=1,2,3,4; my $c; for @a -> $x is rw, $y is rw { $c = -> { $x } if $x==1 }; @a[0]=99; say $c()'
99
$ mutsu -e '...same...'
1
```

The write direction is correct (it was fixed with ADR-0045 row 16):

```
$ mutsu -e 'my @a=1,2,3,4; my $c; for @a -> $x is rw, $y is rw { $c = -> { $x = $x + 1 } if $x==1 }; @a[0]=99; $c(); say @a'
[100 2 3 4]     # matches raku -- the closure READ 99 inside itself
```

Note what that second snippet proves: inside a closure that *writes*, the read
of `$x` sees 99. So the alias is real; it is the **capture** of a closure that
only reads that snapshots. `.kv` shows the same split:

```
$ mutsu -e 'my @a=10,20; my $c; for @a.kv -> $i,$v is rw { $c = -> { $v } if $i==0 }; @a[0]=99; say $c()'
10              # raku: 99
$ mutsu -e 'my @a=10,20; my $c; for @a.kv -> $i,$v is rw { $c = -> { $v = $v+1 } if $i==0 }; @a[0]=99; $c(); say @a'
[100 20]        # matches raku
```

## Why the single-parameter form is fine

`for @a -> $v is rw` binds natively in `exec_for_loop_body`, into `env`, and the
closure capture shares the cell. A multi-parameter binds through the bind-prefix
statements `build_for_bind_stmts` emits, which since ADR-0045 row 16 are
`Stmt::SyntheticBlock([MarkBind, decl])` — a raw bind into a **local slot**
(`SetLocalDecl`). The capture path for a local slot decides between sharing the
cell and snapshotting the value from a static "does this closure write the
name?" analysis, and a read-only body takes the snapshot — which decontainerizes.

That is precisely the hazard CLAUDE.md's "What gain and risk actually mean"
section describes: a by-value capture is only correct if the variable provably
never changes, and here the *element* can change from outside the closure
entirely, which no analysis of the closure body can see.

## Scope

Pre-existing and multi-parameter-wide: it reproduces on `main` with no `.kv`
involved, using the first snippet above. It is the read half of ADR-0045 §1.3
rows 11 and 20 for the multi-parameter shapes; the single-parameter versions of
both are green and pinned in `t/for-loop-element-alias.t`.

## Where to start

`resolve_capture_slot` / `block_captured_scalars` / `compute_owned_captures` in
the closure-capture path (`src/vm/`), specifically the branch that copies a
local slot's *value* into the capture instead of sharing the cell it holds. A
slot holding a `ContainerRef` should always be captured by cell — the whole
point of the cell is that its contents are expected to change. Check the
`t/loop-var-closure-capture.t` and `t/loop-var-nested-closure-freeze.t` pins
(ADR-0027's per-iteration identity) before widening anything: those are what
stop the fix from turning into "every capture shares everything".

## Reproduce

The four snippets above, no fixtures.
