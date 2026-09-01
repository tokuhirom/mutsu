# A `Proxy` from an `is rw` `AT-POS` keeps its STORE, and its capture survives a same-named outer lexical

Closed 2026-09-01 by the TRIAGE regeneration's repro sweep: both defects in
`todo/tickets/proxy-at-pos-store-and-shadowed-capture.md` (filed 2026-08-27
while verifying ADR-0061) no longer reproduce on `main`. No single PR is
credited -- the closure-capture and container work landed 2026-08-27..09-01
covered it -- so this entry pins it rather than attributing it.

## What was wrong

```raku
class B {
    has @.nodes;
    method AT-POS($offset) is rw {
        my $slf = self;
        Proxy.new(
            FETCH => method () { $slf.nodes[$offset] },
            STORE => method ($val) { $slf.nodes[$offset] = $val }
        )
    }
}
my $b = B.new(nodes => ['x','y']);
$b[0] = 'z';
say $b[0];        # raku: z    mutsu (then): x   -- the STORE was a no-op
```

Prepending `my $slf = 1;` at mainline made it worse: `$b[1]` answered `Nil`
instead of `y`, because the Proxy's deferred FETCH captured the wrong `$slf`.
Neither defect depended on the name `self`; they were name-independent
siblings of the ADR-0061 collision.

## Now

`fetch: y` / `after store: z` and, with the shadowing outer lexical,
`shadowed fetch: y` -- matching raku. Pin:
`t/proxy-at-pos-store-and-shadowed-capture.t` (6 assertions, both providers).
`t/lexical-self-vs-invocant.t` had deliberately omitted these two assertions;
the new file carries them.

`todo/tickets/bundle-xml-battery.md` named this ticket as a suspected cause of
the remaining `XML` failures -- re-measure that suite before assuming the two
still-failing files are unrelated.
