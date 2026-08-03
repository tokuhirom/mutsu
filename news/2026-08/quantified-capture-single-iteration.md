# A quantified capture that ran once is still a list

`"a" ~~ /(a)+/` bound `$0` to a bare `Match` instead of a one-element list.
Rakudo listifies every quantified capture regardless of how many iterations ran
— `(a)+`, `(a)*`, `(a)**1` and `(a)**0..1` all give `$0` an `Array`. Only a bare
`?` is exempt: it binds the `Match` itself.

`fold_quantified_captures` had the divergence marked as a deliberate TODO:

```rust
if new_entries <= stride {
    // Exactly one iteration — mutsu keeps the single capture un-folded.
    // TODO: Raku makes `*`/`+` always a List even for one match …
    return;
}
```

The fold now runs for a single iteration too. `descend_folded` reads the token's
own quantifier to decide, so the one case Raku does *not* listify —
`RegexQuant::ZeroOrOne`, the bare `?` — keeps binding a `Match`, while the
otherwise-identical `**0..1` listifies.

## Why it mattered

`Cro::Uri`'s grammar actions walk the numbered capture as a list:

```raku
method pchars($/) {
    my $result = '';
    $result ~= $_<broken> ?? encode-percents(~$_) !! ~$_ for @$0;
    make $result;
}
```

`token pchars { (<[A..Za..z0..9._~…]>+ | '%' <[A..Fa..f0..9]>**2 | …)+ }` matches
a plain path segment in one iteration, so `@$0` was empty and every segment
produced the empty string. `Cro::Uri.parse("http://host/index.shtml").path` came
back as `/`, and `Cro::HTTP::Client` — which builds its request target from
exactly that (`my $target = ($proxy-url ?? ~$url !! $url.path) || '/'`) — sent
every request to `/`. A `route { delegate <*> => $inner }` therefore never
reached the delegated handler.

The unlistified capture was long-standing; it became visible when `make` in a
grammar action started taking effect for this shape, at which point the action's
`''` result replaced the fallback text.

Pinned by `t/regex-quantified-capture-single-iteration.t`.
