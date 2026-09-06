# `List`/`Array`/`Hash` `.WHICH` over-equates: it is content-based, raku's is object identity

Found during the 2026-09-06 neighbourhood sweep for the user-defined-`WHICH`
ticket (`news/2026-09/user-defined-which-decides-object-identity.md`). It is
independent of that fix and reproduces with no user class involved.

## Repro

```raku
say (1, 2).WHICH eq (1, 2).WHICH;   # raku: False   mutsu: True
say [1, 2].WHICH eq [1, 2].WHICH;   # raku: False   mutsu: True
say {a => 1}.WHICH eq {a => 1}.WHICH;  # raku: False   mutsu: True
```

`List`, `Array` and `Hash` are *reference* types in Raku: two separately
constructed containers are never the same object, so their `.WHICH` differs even
when their contents match (`.WHICH` is `ObjAt`-flavoured, not `ValueObjAt`).
mutsu computes a content hash instead, so it reports two distinct containers as
having the same identity.

This is the opposite polarity to the usual identity bug: mutsu is
**over**-equating, which means `===` can answer `True` where raku says `False`.

## Why it may not be a trivial flip

`runtime::utils::value_which_key` deliberately keys `Array` and `Hash` by
allocation pointer (`Array|{:p}`), which is the reference-semantics answer — so
the *internal* keying is already right and only the user-visible `.WHICH`
method disagrees. The suspect is the `"WHICH"` arm of
`src/builtins/methods_0arg/dispatch_core_coerce.rs`, which appears to hash
contents for these types.

Before changing it, check what depends on the current content-based answer:
`values_identical` has its own `Array`/`Hash` arms that already use
`Gc::ptr_eq`, so `===` may already be correct and only the string wrong (the
same shape as the sibling `Pair.WHICH` ticket). Measure `===`, `eqv`, and any
roast assertions on the `.WHICH` spelling first, and beware that an empty `Slip`
is deliberately a singleton in `values_identical`.
