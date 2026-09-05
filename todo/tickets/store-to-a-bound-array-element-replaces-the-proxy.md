# Storing into a `:=`-bound Array element replaces the `Proxy` instead of firing its STORE

ADR-0040 §9.1 says a store to an element that IS a `Proxy` fires that `Proxy`'s
`STORE`. That holds for a `List` element but not for an `Array` element bound
with `@a[0] := $p`, where the store overwrites the binding instead:

```raku
my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });

my $l = (1, $p, 3);
$l[1] = 7;
say $n;                  # 7   -- correct, the STORE fired

my @a = 1, 2, 3;
@a[1] := $p;
say @a[1].VAR.^name;     # Proxy  -- the bind installed the container, per §9.1
@a[1] = 99;
say $n;                  # raku: 99    mutsu: 5   -- the STORE never fired
say @a[1].VAR.^name;     # raku: Proxy mutsu: Scalar -- the Proxy was replaced
```

So the element loses its container identity on the first assignment, which is
exactly the outcome §9.1's "one hook, not dozens of `items_mut()[i] =` sites"
was meant to prevent — the hook evidently sits above the `List` element-assign
dispatch but not above the `Array` one.

Measured 2026-09-05 against `main` at `e4994a3`; verified pre-existing by
running the repro on a stashed tree while working on
`news/2026-09/renderers-fetch-a-nested-proxy.md`, which does not touch the store
side.

`t/proxy-binds-container-not-value.t` passes, so this shape is not among its 24
rows — add it there once fixed.

## Why it is a ticket rather than a one-liner

The right fix is to find the single element-assign dispatch the `Array` spelling
takes and hook it where the `List` spelling is already hooked, rather than adding
a second Proxy check next to it — a second hook is how the "dozens of
`items_mut()[i] =` sites" problem starts again. Locating that shared point, and
confirming that the multi-dim / nested / autoviv element assigns all pass through
it, is the work.

Check the `Hash` spelling (`%h<k> := $p; %h<k> = 99`) at the same time: it is
almost certainly the same gap on the other container.

## Reproduce

The snippet above, no fixtures.
