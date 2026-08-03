# An `our` variable and its package-qualified name are two separate slots

An `our` variable *is* the package-stash entry; the lexical name is an alias for
the same container. In mutsu the declaration stores the value twice — once under
the bare name and once under the qualified one (`compiler/expr_block.rs`: the
`is_our` arm emits `Dup; Dup; SetNamedVar; SetGlobal`) — so a write through
either name leaves the other stale. One direction happens to work, the other
does not:

```raku
our $a = 1;  $GLOBAL::a = 5;   say "$a / $GLOBAL::a";   # raku: 5 / 5   mutsu: 1 / 5
our $b = 1;  $GLOBAL::b++;     say "$b / $GLOBAL::b";   # raku: 2 / 2   mutsu: 1 / 2
our $c = 1;  $GLOBAL::c += 1;  say "$c / $GLOBAL::c";   # raku: 2 / 2   mutsu: 1 / 2
our $d = 1;  $d = 9;           say "$d / $GLOBAL::d";   # raku: 9 / 9   mutsu: 9 / 9  ✓
```

## Why the existing sync misses it

Both directions are already implemented — `sync_our_local_from_qualified` and
`sync_our_package_var_from_local` in `vm/vm_misc_scope.rs` — but they are keyed
on `CompiledCode::our_locals`, a list of `(local slot, qualified name)` pairs the
compiler fills in only when the declaration got a **local slot**. Two cases fall
through it:

- **Mainline scope.** A file-scope `our $a` has no local slot, so `our_locals` is
  empty and the qualified write has nothing to refresh.
- **A write from another compilation unit.** `EVAL '$GLOBAL::c++'` compiles its
  own `CompiledCode`, whose `our_locals` knows nothing about the declaration's
  slot — so even a slot-linked `our` is missed. That is the shape roast hits.

A compile-time list therefore cannot fix it: the link has to be visible at
runtime, from whichever unit performs the write.

## What it blocks

`roast/S02-names/our.t` test 10 under `MUTSU_REAL_TEST=1` — its
`EVAL 'class RT69460 { $GLOBAL::rt69460++ }'` increments the package variable and
the enclosing `our $rt69460` still reads 1. (The rest of that file was freed by
`news/2026-08/block-declares-in-its-own-package.md`.) It is not a `Test`-module
issue at all — the repro above uses no module.

## Shape of the fix

Per ADR-0001's container-representation direction and the project's standing
preference for a *cell* over a snapshot, the sound fix is for the two names to
name **one** container: an `our` declaration binds the lexical name to a shared
`ContainerRef` cell that the package stash also holds, so neither side can go
stale and no sync code is needed.

The cheaper interim — an interpreter-level `our_aliases: HashMap<qualified, bare
env key>` consulted by every qualified write — restores the missing direction but
has a shadowing hazard: a later `my $a` in an inner scope owns the bare env key,
and the alias map would happily write through it. The cell has no such failure
mode, which is why it is the preferred shape even though it touches more code.
