# `next` thrown mid-evaluation of a comma-expression corrupts later `take slip(...)` calls in the same `gather`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Mu.rakudoc:515`).

## Repro

```raku
sub insert($sep, +@list) {
    gather for @list {
        FIRST .take, next;
        take slip $sep, .item
    }
}

say insert ':', <a b c>;
# OUTPUT: «(a : b : c)␤»
```

- raku: `(a : b : c)`
- mutsu (`target/debug/mutsu`): `(a b c)` — every separator is missing, as if every `take
  slip(...)` call after the first iteration only ever took `.item`, dropping `$sep`.

## Isolated minimal repro (no sub/slurpy needed)

```raku
my @list = <a b c>;
my $sep = ':';
say gather for @list {
    FIRST .take, next;
    take slip $sep, .item
}
```
Same divergence: raku `(a : b : c)`, mutsu `(a b c)`.

Removing the `FIRST` phaser (replacing it with an ordinary `if` that also `.take`s and `next`s
on the first element) makes mutsu match raku exactly — so the bug is specific to the `FIRST
.take, next;` phaser form, not to `slip`/`take` in general (a standalone
`gather { take slip $sep, $item }` outside a `FIRST` phaser already works correctly in mutsu).
Wrapping the `slip(...)` argument in explicit parens does not change the outcome either.

## Root cause hypothesis

`FIRST .take, next;` parses as a `Phaser { body: [ArrayLiteral([MethodCall(.take), ControlFlow
(Next)])] }` (confirmed via `--dump-ast`) — i.e. the comma builds an array-literal expression
containing the `.take` call and the `next` control-flow as its two elements, evaluated in a sink
statement. `next` throws mid-construction of that array literal (after `.take()`'s side effect
has already run). This suggests the VM's array-literal-building machinery pushes some internal
state (e.g. an "elements collected so far" stack/buffer) that is not correctly unwound when a
control-flow exception (`next`) is thrown out of the middle of building it — and that leftover
state then corrupts the *next* listop call that uses the same machinery (`slip`'s argument
collection) in later loop iterations.

## Affected files (starting point)

- `src/vm/vm_control_ops.rs` — `next`/control-flow exception handling
- `src/vm/vm_data_ops.rs` — array-literal construction (the "building an array" state that a
  thrown control-flow exception might leave dirty)
- `src/vm/vm_string_regex_ops.rs` / wherever `slip`'s argument-flattening reuses the same
  machinery

## Suggested next step

Use `rust-gdb` to break on the array-literal-construction opcode and step through the `FIRST
.take, next;` case to find what state survives the `next` throw and is still present when the
next `take slip(...)` call runs.
