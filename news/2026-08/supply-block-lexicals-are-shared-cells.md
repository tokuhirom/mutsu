# A supply block's `my` lexical is one binding again

A `supply { … }` block's own `my $x` behaved as a **per-callback snapshot**
rather than one lexical. Each `whenever`/`LAST`/`QUIT` callback captured the live
env by value at registration time and then persisted its own writes against its
own `Sub` identity, so:

```raku
my $s = supply {
    my $acc = '';
    whenever $a -> $v { $acc ~= "a$v"; emit $acc }
    whenever $b -> $v { $acc ~= "b$v"; emit $acc }
};
# raku: a1, a1b2, a1b2a3      mutsu: a1, b2, a1a3
```

and the fold-then-emit idiom lost everything the body accumulated:

```raku
supply {
    my $payload = '';
    whenever $s -> $chunk {
        $payload ~= $chunk;
        LAST emit $payload;      # saw '', the value at block entry
    }
}
```

A *container* mutation (`my $buf = Buf.new; … $buf.append($blob)`) was unaffected,
since nothing rebinds the scalar — which is why the bug stayed hidden for so long
(recorded in `todo/tickets/supply-block-scalar-lexical-invisible-to-last-phaser.md`).

## Fix: a shared cell, not a snapshot

Before a `whenever` registers its callbacks, the enclosing supply body's own `my`
lexicals are promoted to `ContainerRef` cells in the block's env
(`Interpreter::share_supply_block_lexicals`). A cell is captured *by reference*
and overwrites a same-named caller lexical on entry — the `ContainerRef` arm of
the closure-env merge in `resolution_call_sub.rs` already guarantees this — so
every callback of the block reads and writes one binding, without giving up the
lexical-scoping vouch that `owned_lexicals` provides against a same-named lexical
in whatever frame happens to dispatch the callback.

This is deliberately the cell route rather than a wider writeback: a cell tracks
later mutations unconditionally, where a by-value snapshot is only correct under
an analysis that can prove the variable is never written again.

Two kinds of `my`-declared name are excluded, because they are not variables and
a cell in their slot hides the binding:

- the block's emitter parameter, which is dispatched on as an object;
- a `my enum`'s type and variant names (`my_declared_enum_sym`), pinned by
  `t/supply-block-enum-lexical.t`.

Captured *outer* lexicals are also left alone — they belong to the declaring
frame, not to the block.

## Result

Cro's `t/http-request-parser.rakutest` goes from **44 failing subtests to 9**.
The `application/x-www-form-urlencoded` body parser is exactly the broken shape —
it accumulates the body into `my $payload` from a `whenever` and decodes it in a
`LAST` phaser via a sub declared in the same block — so under mutsu every form
body, and every `%`-encoding, multipart and JSON case built on it, decoded as
empty.

Pinned by `t/supply-block-lexical-is-shared.t`.
