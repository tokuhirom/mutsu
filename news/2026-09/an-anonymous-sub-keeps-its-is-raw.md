# An anonymous sub keeps its `is raw`

`is raw` on an **anonymous** sub was parsed and then thrown away:

```raku
my $store = 0;
my $f = sub () is raw {
    Proxy.new(FETCH => { $store }, STORE => -> $, $v { $store = $v })
};
($f()) = 7;
say $store;     # raku: 7   mutsu: Cannot modify an immutable value (sub is not rw)
```

The named spelling (`sub f() is raw { ... }`) worked, and so did `is rw` in
either spelling, which is what kept this hidden. `SubTraits` had already parsed
the trait; it had nowhere to go. `Expr::AnonSub` and `Expr::AnonSubParams` — the
two nodes every closure literal lands on — carried only `is_rw`, so the anon-sub
parser wrote `is_rw: traits.is_rw` and dropped `traits.is_raw` on the floor. The
pooled `Stmt::SubDecl` the compiler then built hardcoded `is_raw: false`, and the
`SubData` the VM made from it therefore never reached the rw-capability oracle
(`is_rw || is_raw || a return-rw in the body`) that
`assign_callable_lvalue_with_values` asks before writing through a routine's
returned container.

Both AST nodes have an `is_raw` field now, the parser records the trait, and the
compiler threads it into the pooled declaration and into
`compile_routine_closure_body`'s rw-tail decision, exactly as it already did for
`is_rw`. The ~50 other construction sites are synthesized closures and pass
`false`; the three that destructure-and-rebuild an anonymous sub (the unary-wrap
helper, the invocant-alias binder, and the proto-dispatch rewriter) carry the
flag through, as does the lift of one candidate out of an anonymous `multi sub`.

## What it unblocks

`runtime/container_element_proxy.rs` — the native `AT-KEY` base candidate
`nextcallee` hands a container subclass's own override — had to be written with
`$`-sigiled parameters and `is rw` to step around this and around
[#7879](https://github.com/tokuhirom/mutsu/issues/7879). Both workarounds are
gone: it is now spelled the way Rakudo spells `Baggy::AT-KEY`.

```raku
sub (\obj, \key) is raw {
    Proxy.new(
        FETCH => { obj.__mutsu_container_at_key(key) },
        STORE => -> $, \value { obj.__mutsu_container_assign_key(key, value) }
    )
}
```

A prelude that diverges from the upstream spelling is a small private dialect of
exactly the kind BATTERIES.md §1 warns about — every divergence is a place where
mutsu's behaviour can drift from rakudo's without a test noticing. Keeping this
one honest cost one real compatibility fix.

Pinned by `t/routines/closure/anon-sub-raw-lvalue-return.t`, which covers the
named/anonymous × `is raw`/`is rw` matrix, the sigilless-parameter capture, and
the negative case (an anonymous sub with neither trait still refuses the write).
All seven assertions were verified against rakudo.

One spelling is deliberately *not* pinned: `sub is raw { ... }` with no
signature. mutsu accepts it, but rakudo parses `is` as the routine's name and
rejects the block, so there is no correct behaviour to assert.
