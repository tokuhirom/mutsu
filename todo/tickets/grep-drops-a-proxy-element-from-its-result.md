# `grep` drops a `Proxy` element from its result

`.grep` over a list holding a `Proxy` element loses that element even when the
block returns `True` for it. `.map` over the same list is correct, so this is
`grep`'s result-collection, not the element read.

```raku
my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
my $l = (1, $p, 3);

say $l.grep({ True }).elems;        # raku: 3   mutsu: 2
say $l.grep({ $_ > 2 }).join(",");  # raku: 5,3 mutsu: 3
```

The block *does* see the element, and sees it FETCHed — inserting a
`say $_.raku` in the block prints `1`, `5`, `3`, three times — and `.map` agrees
with rakudo throughout:

```raku
say $l.map({ $_ + 1 }).join(",");          # 2,6,4        -- correct
say $l.map({ ($_ > 2).Str }).join(",");    # False,True,True -- correct
```

So the block is invoked for the Proxy element, returns a true value, and the
element still does not reach the result.

Measured 2026-09-05 against `main` at `e4994a3`, and unchanged by
`news/2026-09/renderers-fetch-a-nested-proxy.md` (which deliberately leaves
`map`/`grep` out of the renderer set — they bind the element container, Proxy
included, per ADR-0045). Verified pre-existing by running the repro on a stashed
tree.

## Why it is a ticket rather than a one-liner

The likely shape is that `grep`'s collection step tests the *element* for
truthiness somewhere in addition to the block's answer, or filters on a property
of the value that a `Proxy` fails — a `Proxy` element would then be silently
dropped regardless of what the block said. That has to be found before it can be
named: `grep` has several implementations (a native fast path, the
`CallMethodMut` path, and the lazy-pipe stage), and the fix belongs in whichever
one both spellings share, not in the first one reproduced.

Worth checking at the same time whether `first`, `sort`, and `unique` — the other
element-filtering natives — have the same hole, since they would share whatever
predicate is at fault.

## Reproduce

The two one-liners above, no fixtures.
