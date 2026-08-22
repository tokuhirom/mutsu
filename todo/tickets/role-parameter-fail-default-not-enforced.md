# A role parameter's `fail(...)` default expression is never evaluated/enforced

Discovered via the doc-diff harness on `raku-doc/doc/Language/typesystem.rakudoc` (around line
644).

## Repro

```
role R[$p = fail("boom")] {};
my $i = 1 does R;
say $i.^name;
```

- raku: throws (a `CATCH` fires; the exception is `X::Role::Instantiation` wrapping the
  `X::AdHoc` from `fail("boom")`) — composing `R` without supplying `$p` is an error
- mutsu: succeeds silently; `$i.^name` is `Int+{R}`

Note: the doc's `# OUTPUT:` for the exception's exact type has drifted (`X::AdHoc` vs. current
raku's `X::Role::Instantiation`-wrapped form) — that specific text mismatch is drift, not part
of this finding. The real bug is that mutsu doesn't throw *at all*.

## Root cause guess

Role parameters with a default expression are presumably only evaluated when actually
referenced inside the role body, but a default that itself calls `fail(...)` needs to be forced
(evaluated) at composition time when the parameter isn't explicitly supplied, precisely so it
*can* raise. mutsu likely treats an omitted role parameter as simply unbound/lazy without ever
forcing its default expression during `does`/parameterized composition.

## Affected files (starting point)

- `src/runtime/class.rs` — parametric role composition, default-parameter evaluation

## Suggested next step

Check whether role parameter defaults are evaluated eagerly at all during `does R` (without
explicit parameterization) — a defaults that's a plain literal (e.g. `$p = 5`) is a good control
case to confirm defaults are evaluated at all, then narrow down why a `fail(...)`-producing
default specifically doesn't propagate its exception.
