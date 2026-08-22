# `my $x := sub-call-returning-Proxy();` loses the Proxy's writability

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Proxy.rakudoc:17`).

## Minimal repro

```raku
sub double() {
    my $storage = 0;
    Proxy.new(
        FETCH => method ()     { $storage * 2    },
        STORE => method ($new) { $storage = $new },
    )
}
my $doubled := double();
$doubled = 4;
say $doubled;
```

- `raku`: `8`
- `mutsu` (`target/debug/mutsu`): dies with
  `Cannot assign to a readonly variable (doubled) or a value`.

Both these narrower variants work correctly, isolating the bug to the specific combination
of `:=` binding + a sub-call RHS whose returned value is a `Proxy`:

- `my $p = Proxy.new(...); $p = 4; say $p;` → `8` (direct assignment, no sub call: OK)
- `my $doubled := Proxy.new(...); $doubled = 4; say $doubled;` → `8` (`:=` binding directly
  to a `Proxy.new(...)` literal, no sub call: OK)
- Removing `is rw` from `sub double()` makes no difference — the failure reproduces either
  way, so this is not specifically about `is rw` return semantics.

## Root cause hypothesis

Binding (`:=`) to the result of a sub *call* appears to go through a different code path
than binding directly to an expression — likely one that resolves/copies the sub's return
value into a plain (readonly) binding slot rather than preserving the returned `Proxy`
object's own FETCH/STORE dispatch. The Proxy's `STORE` method should still be reachable
through the bound name after the call returns; instead mutsu's readonly-binding error fires,
meaning the bind ends up wrapping something it treats as an immutable value rather than the
live Proxy container.

## Affected files (starting point)

- Wherever `:=` binding to a function-call expression is compiled/executed (look at how the
  VM's bind-op handles a `Call`/`MethodCall` RHS vs. a plain expression RHS) — needs to check
  whether a returned `Proxy` value is unwrapped/copied before the bind instead of being bound
  through directly.
