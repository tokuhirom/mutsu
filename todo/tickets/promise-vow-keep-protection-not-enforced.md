# `Promise.vow` doesn't protect against a second `.keep`/`.break` via the original Promise

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/concurrency.rakudoc:213`).

## Repro

```raku
sub get_promise {
    my $promise = Promise.new;
    my $vow = $promise.vow;
    Promise.in(10).then({$vow.keep});
    $promise;
}

my $promise = get_promise();

# Will throw an exception
# "Access denied to keep/break this Promise; already vowed"
$promise.keep;
CATCH { default { say .^name, ': ', .Str } };
# OUTPUT: «X::Promise::Vowed: Access denied to keep/break this Promise; already vowed␤»
```

- raku: `X::Promise::Vowed: Access denied to keep/break this Promise; already vowed`
- mutsu (`target/debug/mutsu`): no output at all — the `CATCH` block never fires, meaning
  `$promise.keep` neither throws nor is reported.

## Analysis

Calling `.vow` on a `Promise` should mark it as "vowed" — from then on, only that specific `Vow`
object (not the `Promise` itself) is allowed to `.keep`/`.break` it; attempting `.keep`/`.break`
directly on an already-vowed `Promise` should throw `X::Promise::Vowed`. mutsu appears not to
implement this protection at all: `$promise.keep` silently does nothing (or silently succeeds)
instead of throwing.

## Affected files (starting point)

- `Promise.vow`/`Promise.keep`/`Promise.break` implementation (concurrency runtime module) — needs
  a "vowed" flag set by `.vow` and checked by `.keep`/`.break` on the `Promise` object itself
  (raising `X::Promise::Vowed` when set).
