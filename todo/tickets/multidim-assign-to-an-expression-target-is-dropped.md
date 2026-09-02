# A multi-dim assignment to an EXPRESSION target is silently dropped

When the left-hand side of a multi-dimensional subscript assignment is not a
plain variable, the compiler falls back to `MultiDimIndexAssignGeneric`, which
pops the target *value* off the stack, mutates that throwaway copy, and drops
it. Nothing is written:

```raku
my %o; %o<inner>{1;2} = 5; say %o.raku;   # raku: {:inner(${"1" => ${"2" => 5}})}
                                          # mutsu: {}
```

The positional spelling loses the write the same way (`%o<inner>[0;1] = 5`),
though raku rejects that one outright (`Cannot resolve caller ASSIGN-POS`), so
the associative spelling above is the case that matters.

Found 2026-09-02 while making `%h{1;2} = 5` work (the associative multi-dim
walk, `news/2026-09/associative-multidim-subscript.md`). It is a *separate*
gap: the named-target opcode (`MultiDimIndexAssign`) resolves and writes back
its variable, and only the generic arm is broken.

## Why it is not a one-liner

The obvious fix -- compile the target under `scalar_bind_autovivify` so it
yields a shared `ContainerRef` cell instead of a read value -- was tried and
does **not** work: `%o<inner>` still compiles to a plain read (the flag reaches
`Expr::Index`, but the missing hash entry is not vivified into a cell there),
so the generic op still gets a detached `Any`. Making it work needs the
single-subscript autovivification path (`fresh_autoviv_container` /
`assign_into_nested_container` in `src/vm/vm_var_assign_index_named.rs`) to be
reachable for an arbitrary expression target, which is the same "lvalue an
arbitrary subscript chain" machinery several other tickets want.

## Repro

```
raku  -e 'my %o; %o<inner>{1;2} = 5; say %o.raku'
mutsu -e 'my %o; %o<inner>{1;2} = 5; say %o.raku'
```
