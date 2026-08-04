# A Cro route handler's captured `atomicint` counter does not count

A `route` block that keeps per-application state in an `atomicint` and bumps it
from a handler gets the wrong number out:

```raku
my $application = route {
    my atomicint $i = 0;
    get -> 'counter' {
        my $n = ++⚛$i;
        note "  [handler] run #$n";
        content 'text/plain', $n.Str;
    }
}
```

Two requests to `/counter` print `run #0` **both times** and answer `0` both
times; `raku` prints `run #1` / `run #2` and answers `1` / `2`
(`tmp/counter1.p6`).

## It is a Heisenbug: one statement earlier and it works

Putting *any* statement before the increment fixes it. `tmp/counter2.p6` is the
same server with a `note` first:

```raku
get -> 'counter' {
    note "  [handler] before: name=", $i.^name, " read=", (try ⚛$i).raku;
    my $n = ++⚛$i;
    …
}
```

and it counts correctly — `read=0` → `n=1` → `read=1`, then `read=1` → `n=2`.
So the captured container is reachable and the atomic ops work; something about
the increment being the **first statement of the handler body** loses it.

That is the same first-statement/sink-position shape as the `ExecCall` builtin
shadow fixed in `news/2026-08/imported-sub-shadows-a-builtin-in-sink-position.md`,
which is worth checking first — but note this reproduces *with* that fix in.

## Synthetic repros are all green

Do not chase a smaller repro by guessing; these were tried and pass on mutsu:

- `tmp/atomic1.p6` — `++⚛`/`⚛`/post-increment at file scope, in a sub, and in a
  closure.
- `tmp/atomic2.p6` — an `atomicint` declared inside a block passed to a sub,
  captured by a handler closure registered from that block and called later
  (the Cro `route`/`get` shape), plus the plain-`Int` control.
- `tmp/atomic3.p6` — the same with the increment as the **first** statement of
  the handler versus with a statement before it.

Grow `tmp/counter1.p6` down instead.

## Blast radius

The vendored Cro suite's `http-middleware.rakutest` subtest 4
("Request/response middleware using Cro::HTTP::Middleware::RequestResponse"),
whose `/counter` route is exactly this shape: `is await($resp.body-text), '1'`
gets `'3'`. That is the only failing subtest left in that file.
