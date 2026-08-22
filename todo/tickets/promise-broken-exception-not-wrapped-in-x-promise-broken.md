# A broken `Promise`'s exception isn't wrapped/mixed with `X::Promise::Broken`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/concurrency.rakudoc:49`
and `:85`).

## Repro 1 — `.break('str')` should mix `X::Promise::Broken` into the resulting exception

```raku
my $p2 = Promise.new;
$p2.break('oh no');
say $p2.status;         # OUTPUT: «Broken␤»
say $p2.result;         # dies, because the promise has been broken
CATCH { default { say .^name, ': ', .Str } };
# OUTPUT: «X::AdHoc+{X::Promise::Broken}: oh no␤»
```

- raku: `X::AdHoc+{X::Promise::Broken}: oh no` (an anonymous mixin combining `X::AdHoc` with the
  `X::Promise::Broken` role)
- mutsu (`target/debug/mutsu`): `X::AdHoc: oh no` (plain `X::AdHoc`, no `X::Promise::Broken` mixin)

## Repro 2 — a chained `.then` on a broken Promise should wrap the cause with "Tried to get the result of a broken Promise" / "Original exception:"

```raku
my $promise1 = Promise.new();
my $promise2 = $promise1.then(-> $v { say "Handled but : "; say $v.result});
$promise1.break("First Result");
try $promise2.result;
say $promise2.cause;
```

- raku:
  ```
  Handled but : 
  Tried to get the result of a broken Promise
    in block  at ... line 2
  
  Original exception:
      First Result
        in block  at ... line 2
  ```
- mutsu:
  ```
  Handled but : 
  First Result
    in block <unit>
  ```

(The doc-diff harness bucketed repro 2 as `raku-drift-from-doc` because raku's *current* output
no longer matches the doc's stated `# OUTPUT` — that bucketing is correct for the doc-vs-raku
comparison, but re-verified directly here: mutsu's own output diverges from *raku's actual current
behavior* in a real, substantive way — the "Tried to get the result of a broken Promise" /
"Original exception:" wrapper is missing entirely, not just cosmetically different — so it is
filed here as a real bug rather than skipped.)

## Analysis

Both repros show the same root cause: when a `Promise` is broken (via `.break(...)` or by an
upstream broken Promise propagating through `.then`), raku wraps/exposes the cause through an
`X::Promise::Broken`-flavored exception (either directly mixed with `X::AdHoc` for a plain
`.break('str')`, or — when the caller tries to read `.result` on an already-broken Promise —
wrapped in a "Tried to get the result of a broken Promise" message chaining to the original cause
via "Original exception:"). mutsu's Promise-break path never constructs this
`X::Promise::Broken`-flavored wrapper; it surfaces the raw broken-with value/exception directly.

## Affected files (starting point)

- Wherever `Promise.break()`/`.then()`-on-broken-Promise resolution builds the resulting
  exception/cause (likely `src/runtime/` concurrency/Promise implementation) — needs an
  `X::Promise::Broken` role mixin path.
