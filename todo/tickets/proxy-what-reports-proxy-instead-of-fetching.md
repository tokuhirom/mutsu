# `.WHAT` on a `Proxy` reports `(Proxy)` instead of FETCHing first

## Symptom

A `Proxy` is a container: every read of it — including the one `.WHAT` performs — has to
go through `FETCH` and answer about the *fetched value*, not about the container. mutsu
answers about the container.

```raku
my $p = Proxy.new(FETCH => method () { "hi" }, STORE => method ($v) { });
say $p.WHAT;   # raku: (Str)     mutsu: (Proxy)
say $p;        # raku: hi        mutsu: hi   (correct — plain stringification does FETCH)
```

`say $p` is already right, so the deref is wired up on the ordinary read path; only the
type-introspection path answers before dereferencing. `.^name` is worth checking at the
same time, as are `.defined`, `.DEFINITE` and `.raku`.

## Root cause (not yet located)

Not investigated beyond the repro. The likely shape is that the `WHAT` handler is one of
the metaobject-ish 0-arg natives dispatched *before* the generic container deref, in the
same family as the `"self" | "clone" | "WHERE" | "WHICH" | "sink" | "item" | "serial"`
early-dispatch lists in `src/builtins/native_method_row.rs` and
`src/vm/vm_call_method_ops.rs`. Whatever chokepoint FETCHes for the `say $p` path is the
one `.WHAT` needs to run behind too.

## Why it is its own ticket

It was found alongside the `my $self` / invocant collision
(ADR-0061, `news/2026-08/lexical-self-has-its-own-env-key.md`) because both surfaced
while measuring the `XML` battery's `Proxy`-based `AT-POS`. It is fully independent of
that bug: it reproduces with a `Proxy` that closes over nothing at all, and it survived
the ADR-0061 fix unchanged.

## Repro

```sh
cargo build
timeout 10 ./target/debug/mutsu -e 'my $p = Proxy.new(FETCH => method () { "hi" }, STORE => method ($v) { }); say $p.WHAT'
# mutsu: (Proxy)   raku: (Str)
```
