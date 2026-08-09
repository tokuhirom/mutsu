# `.UInt` on a negative value throws instead of returning a soft Failure

## Repro

```
$ raku -e 'my $x = "-1".UInt; say $x.^name; say "alive"'
Failure
alive
$ target/debug/mutsu -e 'my $x = "-1".UInt; say $x.^name; say "alive"'
Coercion to UInt out of range. Is: -1, should be in 0..^Inf
  in block <unit> at -e line 1
```

rakudo's out-of-range `.UInt` coercion returns an (unthrown) `Failure`; it only
explodes when the Failure is sunk or used. mutsu throws immediately at the
coercion site.

## Impact

`Cro::HTTP` `t/http-router.rakutest` test 83 ("Route with optional UInt named
arg for query parameter doesn't match negative values"): the router's generated
unpack code runs `%unpacks{Q[page]} = .UInt with $req.query-value(Q[page])` and
relies on the Failure flowing into the capture so the subsequent
`$han.signature.ACCEPTS($cap)` bind check rejects the route (→ 404). Under
mutsu the throw escapes the route matcher and kills the whole test file at that
point (`rc` stays 0 but the remaining plan is abandoned mid-file).

As of the `named_names` fix (see
`news/2026-08/parameter-named-names-plain-named.md`) this is one of only two
remaining `http-router.rakutest` failures.

## Notes

- Check the sibling coercions while here: rakudo `"abc".Int` is also a Failure,
  `(-1).UInt` likewise; the fix should cover the shared out-of-range/parse-fail
  coercion path, not special-case UInt.
- The error message text mutsu prints is already rakudo-compatible — the bug is
  only throw-vs-Failure timing.
