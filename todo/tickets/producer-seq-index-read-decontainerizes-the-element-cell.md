# Indexing an element producer's `Seq` drops the element container

Found by ADR-0036 slice 5's sweep (2026-09-01), which compared 69 rows of the
whole element-container surface against `raku`. Five of them are this one bug,
three of them silent no-ops. **Not a regression** — verified identical on `main`
at `f03b85978`, i.e. before ADR-0036 slice 4.

## Symptom

`.values` / `.kv` / `.pairs` hand out the element's `Scalar` container
(ADR-0036 slice 3). Consuming that result with a `for` loop keeps the container,
but **subscripting it does not**:

```
$ raku  -e 'my @a = <A B>; say (@a.values)[0].VAR.^name'          # Scalar
$ mutsu -e 'my @a = <A B>; say (@a.values)[0].VAR.^name'          # Str      X

$ raku  -e 'my @a = <A B>; (@a.values)[0] = "x"; say @a'          # [x B]
$ mutsu -e 'my @a = <A B>; (@a.values)[0] = "x"; say @a'          # [A B]    X  silent

$ raku  -e 'my @a = <A B>; (@a.kv)[1] = "x"; say @a'              # [x B]
$ mutsu -e 'my @a = <A B>; (@a.kv)[1] = "x"; say @a'              # [A B]    X  silent

$ raku  -e 'my @a = <A B>; my $c := (@a.values)[0]; $c = "x"; say @a'   # [x B]
$ mutsu -e 'my @a = <A B>; my $c := (@a.values)[0]; $c = "x"; say @a'   # [A B]  X  silent
```

Adjacent shapes that *do* work, and are the useful contrast:

```
my @a = <A B>; say @a.values.head.VAR.^name            # Scalar in both  (.head keeps it)
my @a = <A B>; say @a.pairs[0].value.VAR.^name         # Scalar in both  (.value returns the cell)
my @a = <A B>; (@a[0]:kv)[1] = "x"; say @a             # [x B] in both   (subscript adverb)
my @a = <A B>; for @a.values -> $v is rw { $v = "x" }  # works in both
```

So it is specifically the **positional subscript of the producer's `Seq`**, not
the producer and not the consumer.

## Root cause

`Interpreter::exec_index_op_with_positional` (`src/vm/vm_var_index_ops.rs`)
normalizes a `Seq` receiver to an `ArrayKind::List` array before the index
match:

```rust
if let ValueView::Seq(items) = target.view() {
    ...
    target = Value::array_with_kind(
        crate::value::Value::array_arc(items.to_vec()),
        crate::value::ArrayKind::List,
    );
}
```

The `(Array, Int)` arm then reads through `resolve_array_entry`, which is *the*
decontainerization chokepoint — the thing that keeps `.raku` / `.WHAT` / `.gist`
honest when a slot holds a `ContainerRef`. It does its job here too, and that is
exactly the problem: for a producer `Seq` the cell *is* the answer.

Note the receiver shape matters because the two forms compile differently.
`(@a.values)[0]` emits `CallMethodMut` then `OpCode::Index`, which is the path
above; `my \s = @a.values; s[0]` goes through the name-keyed index path and
keeps the cell on the read (though its *write* is lost separately, row 67).

## Why this is not a one-line fix

Two halves, and the first one has real blast radius:

1. **Read.** Skipping `resolve_array_entry` for a `Seq`-sourced subscript hands
   a `ContainerRef` to every consumer of `seq[i]`. ADR-0036 §6 names this as the
   change with real blast radius, and §5 Q4/Q5 exist to bound it. It probably
   has to be narrowed to "the `Seq` came from an element producer", which the
   value does not currently record.
2. **Write.** `(@a.values)[0] = "x"` is an index-assign whose target is an
   expression, not a named variable; making the read hand out a cell does not by
   itself make that store write *through* it.

## Suggested acceptance rows

The five rows above, plus the four contrast rows, as a `t/` file checked against
real raku. Note `(@a.values)[0].WHAT` must stay `(Str)` — raku decontainerizes
there — so the pin has to assert both directions.
