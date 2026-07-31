# Forward-captured `&`-sigil lexical is snapshotted as Nil — blocks cbor-decode

`CBOR::Simple`'s decoder builds mutually-recursive decode closures:

```raku
my &decode-array = {
    ...
    my @ = (^read-uint).map(&decode)   # <- &decode read here
};
...
my &decode = { ... };                  # <- assigned AFTER the captures above
```

Under mutsu, calling `cbor-decode(...)` dies with
`Cannot map a Nil to a Range, it's not callable.` — the closure captured
`&decode` **by value at creation time** (Nil, since the assignment comes
later), and never sees the later assignment. raku captures the variable, so
the forward reference works.

This is the `&`-sigil twin of the value-typed-scalar snapshot bug fixed in
the nqp-ops slice (`vm_register_ops.rs` — captured `my int` / `Int:D is rw`
now box into shared cells). `&`-vars are explicitly skipped from cell
boxing (`s.starts_with('&') { continue; }` in the boxing loop), presumably
because a `ContainerRef` in a code-var slot would need every call path to
deref the cell.

## Impact

- `cbor-decode` (any input) — the last blocker for a full CBOR::Simple
  round-trip; `cbor-encode` already produces byte-correct output.
- CBOR::Simple is otherwise only a *load-time* dep of Log::Timeline (whose
  outputs are activated by env vars), so `use Cro::HTTP::Router` and the
  Cro server path do NOT need this — it gates CBOR *functionality* only.

## Repro

```raku
my &f;
my &g = -> { f() };
&f = -> { 42 };     # or: my &f = ... after &g's creation
say g();            # raku: 42; mutsu: "Nil is not callable" (or similar)
```

(Verify the exact minimal shape — the CBOR one goes through `.map(&decode)`
argument position.)
