# A user `trait_mod:<is>` that matches nothing does not consume the trait

In Raku the built-in `trait_mod:<is>` candidates live in the same multi as any
user-declared one, so a user candidate whose signature does not match simply
does not claim the trait — dispatch falls through to the built-in handling, and
an unrecognised trait still ends up at `X::Comp::Trait::Unknown`.

mutsu routed **every** `is` trait through user multi-dispatch as soon as one
user candidate existed anywhere, and reported the dispatch failure verbatim:

```
$ mutsu -e 'multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) { };
            try { EVAL q{my $a is definitely-invalid = 5} }; say $!.^name'
X::Multi::NoMatch          # raku: X::Comp::Trait::Unknown
```

Merely importing `Test` was enough to trigger it, because `Test.rakumod`
exports exactly that candidate — so under the real module every
`throws-like '… is <unknown-trait> …', X::Comp::Trait::Unknown` failed.

The variable-trait path now falls back to the built-in answer when dispatch
returns the *no candidate matched* verdict for `trait_mod:<is>` itself. The
check is deliberately narrow (`Cannot resolve caller trait_mod:<is>` /
`No matching candidates for proto sub: trait_mod:<is>`): an error raised from
*inside* a handler that did match is a real error and still propagates, which
the pin asserts.

`t/variable-traits.t` is green under the aliased upstream `Test` module as a
result (`todo/tickets/vendor-real-test-module.md`).

Pinned by `t/user-trait-mod-does-not-consume-every-trait.t`, whose 6 assertions
are green under `raku` too.

## Not covered

The same shape for *routine* traits is a different code path and mutsu does not
dispatch there at all today — `multi sub trait_mod:<is>(Routine:D $r,
:$explodes!) { die }` followed by `sub bad() is explodes { }` neither runs the
handler nor errors. That is out of scope here; the pin tests the variable-trait
path this change touches.
