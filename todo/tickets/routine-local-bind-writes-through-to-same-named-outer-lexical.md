# A `:=` bind of a routine-local `my $x` writes through to a same-named outer lexical

## Symptom

Binding a routine-local declaration to an alias and writing through the alias leaks the
write into an enclosing scope's *same-named* lexical, which the routine never touched:

```raku
my $q = "OUT";
sub m { my $q = 5; my $r := $q; $r = 9; $q }
m();
say $q;      # raku: OUT     mutsu: 9
```

It reproduces identically with a `method` body instead of a `sub`, and with the
enclosing declaration at mainline or inside a block. The direct forms are fine — replace
`my $r := $q; $r = 9` with `$q = 9` (or `$q++`, `$q += 4`) and the outer `$q` keeps its
value — so it is specific to the write travelling through the `:=` alias.

Name-independent: `$q` above is an ordinary lexical, so this is not the `$self`/invocant
collision fixed by ADR-0061 (it was found while writing that fix's pinning test,
`t/lexical-self-vs-invocant.t`, and reproduces on any name).

## Suspected shape

The routine-local `my $q` and the mainline `$q` share a name, and the routine body's
`my $q` is compiled by a child compiler that resolves `$q` as a free variable when it is
not in its own `local_map`. The `:=` alias then appears to resolve its target by *name*
rather than through the declaration's own slot/cell, so the write lands on the outer
binding. Candidate code: the `Bind`/`AssignOp::Bind` path in `src/compiler/stmt.rs` and
the `ContainerRef`/cell plumbing behind it; compare
[ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md)
(a closure's free variable resolves to its own binding — still `Proposed`) and
[deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md](../deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md),
which is the same family (`:=` bind propagating into ancestor frames) for recursion.

This may well be a narrower, easier-to-attack instance of that `deep/` ticket; check
whether one fix covers both before treating them as separate work.

## Repro

```sh
cargo build
timeout 10 ./target/debug/mutsu -e 'my $q = "OUT"; sub m { my $q = 5; my $r := $q; $r = 9; $q }; m(); say $q'
# mutsu: 9    raku: OUT
```
