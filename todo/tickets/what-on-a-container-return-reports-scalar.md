# `.WHAT` / `.^name` on a container returned by a routine reports `Scalar`

An lvalue return hands the caller a `ContainerRef`. Every consumer
decontainerizes it — `say`, `.raku`, `.gist`, `.Str`, `.elems`, arithmetic, list
context, parameter binding, and now a plain `=` store — with exactly two
exceptions, `.WHAT` and `.^name`:

```raku
my @a = 1, 2, 3;
sub e() { return-rw @a[0] }
say e().WHAT;      # raku: (Int)   mutsu: (Scalar)
say e().^name;     # raku: Int     mutsu: Scalar

my $v = 1;
sub f() { return-rw $v }
say f().WHAT;      # raku: (Int)   mutsu: (Scalar)
```

Reading the same container through a *variable* is correct
(`my $r := e(); say $r.WHAT` is `(Int)`), because `GetLocal` dereferences. Only
the invocant that comes straight off the call is affected.

## Root cause

`src/vm/vm_call_method_ops.rs` (the `method != "VAR"` block that decontainerizes
a `ContainerRef` invocant) has a deliberate exception:

```rust
ValueView::ContainerRef(_) => {
    if args.is_empty() && matches!(method, "^name" | "WHAT") {
        // ... push Value::package("Scalar") / Value::str("Scalar")
        return Ok(());
    }
    target.deref_container()
}
```

The exception exists so that `$obj.attr.VAR.^name` answers `"Scalar"`, which is
what Rakudo says. That chain reaches it because `.VAR` on an **attribute** returns
the raw cell (the `want_ref` accessor path, same file), unlike `.VAR` on a
*variable*, which returns a `Scalar`-classed reflection `Instance` (ADR-0057) and
never needs this arm. So the intercept is keyed on the wrong thing: it fires for
*any* bare-`ContainerRef` invocant, not only for one the user asked for with
`.VAR`.

## Why it is not a one-liner

Removing the intercept outright breaks `$obj.attr.VAR.^name` — and `.VAR.^name` /
`.VAR.WHAT` appear in ~14 `t/` files and several roast files. The fix needs a way
to distinguish "this container is a `.VAR` reflection" from "this container is an
lvalue return". Two candidates:

1. **A static compiler marker.** `X.VAR.^name` is syntactically visible
   (`MethodCall { target: MethodCall { name: "VAR" }, name: "^name" }`), so the
   compiler could mark the outer `CallMethod` and the VM could gate the intercept
   on that marker. Cannot leak (one-shot, emitted per site), but adds an opcode
   or a flag.
2. **Make attribute `.VAR` return a reflection object too**, matching the
   variable case, so a bare `ContainerRef` never reaches `.WHAT` as a `.VAR`
   result. Cleaner, but it is ADR-0057 territory and changes `.VAR` identity
   semantics for attributes and elements.

## Status

Pre-existing: it already applied to `return-rw @a[0]`, to `.grep` rw aliases and
to `:=`-bound slots extracted via `.head`/`.first`.
`news/2026-08/return-rw-produces-first-class-containers.md` widened its reach to
`return-rw $v`, and deliberately left it alone — the accompanying test
(`t/return-rw-container-values.t`) pins every other invisibility property and
omits `.WHAT` on the call result for this reason.

## Affected files

- `src/vm/vm_call_method_ops.rs` — the intercept (search for
  `containerref-scalar-meta`).
- `src/runtime/methods_introspect.rs` — `dispatch_what`, which already handles
  `ContainerRef` correctly and is simply never reached for this shape.
- `docs/adr/0057-var-reflection-identity-cell-address.md` — the `.VAR` model.
