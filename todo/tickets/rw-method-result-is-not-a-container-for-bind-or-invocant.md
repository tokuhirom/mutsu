# An `is rw` *method* returns a container when assigned to, but not when bound or used as an lvalue invocant

Measured 2026-09-06 against raku v2026.07 and a debug `mutsu` built from `main`
at `30d6754f5` (plus ADR-0067's E6 producer, which does not reach this shape).

```raku
class C { has $.v is rw; method acc is rw { $!v } }
my $c = C.new(v => 42);
```

| Consumer | raku | mutsu |
|---|---|---|
| `$c.acc = 9` | `9` | `9` |
| `my $x := $c.acc; $x = 9` | `9` | **dies**: `Cannot assign to an immutable value` |
| `sub g($y is rw) { $y = 9 }; g($c.acc)` | `9` | **dies**: `Parameter '$y' expects a writable container ...` |
| `$c.acc.snitch = 9` (the E6 shape) | `42` then `9` | **dies**: `X::Assignment::RO: cannot assign through .snitch on non-instance` |
| `self!p.snitch = 9` for `method !p is rw { $!v }` | `42` then `9` | the same refusal |

## Why the E6 producer does not cover it

ADR-0067's E6 producer emits `MarkAccessorRefContext` before an lvalue call's
invocant, and that marker is consumed by `try_fast_accessor_read`
(`src/vm/vm_call_method_ops.rs`), which by construction only serves a **public
attribute accessor** — it bails as soon as
`resolve_user_method_or_accessor` answers `UserMethod` rather than `Accessor`.
A user-written `is rw` method is exactly the `UserMethod` case.

The `$c.acc = 9` row works through a completely different route (the lvalue
assignment path calls the method and writes through the container its rw tail
returns, ADR-0059). So the machinery exists; what is missing is a producer that
runs the rw method **for a plain read** and hands its container back when the
read is in a container-wanting context (`:=`, an `is rw` argument, an lvalue
invocant).

The `:=` row is the cheapest entry point and the one to measure first: it is the
same context `MarkAccessorRefContext` already flags, so the question is only
whether the flagged dispatch can be routed to a container-mode method call
instead of `try_fast_accessor_read`'s accessor-only fast path. Note that doing so
means *calling* the method in a context where mutsu currently does not, so a
non-rw method must keep going through the plain read — gate on
`Interpreter::method_is_rw_capable` (ADR-0067 slice 2's oracle), not on the shape
of what comes back.

## Repro

```raku
class C { has $.v is rw; method acc is rw { $!v } }
my $c = C.new(v => 42);
my $x := $c.acc;
$x = 9;
say $c.v;          # raku: 9    mutsu: dies "Cannot assign to an immutable value"
```
