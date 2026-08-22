# `@arr[$range-or-array-var]` doesn't flatten into `for` iteration when the subscript is a variable

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Range.rakudoc:80`).

## Root cause

`@arr[0..2]` (a **literal** Range subscript) correctly slips its elements one-per-iteration
into a `for` loop. But when the same Range (or an Array holding the same indices) is first
bound/assigned to a variable and *that variable* is used as the subscript, the result is
treated as a single list item instead of flattening:

```raku
my @numbers = <4 8 15 16 23 42>;
.say for @numbers[0..2];        # raku AND mutsu: 4 / 8 / 15 (3 lines)      -- matches

my $range := 0..2;
.say for @numbers[$range];      # raku: 4 / 8 / 15 (3 lines)
                                 # mutsu: "(4 8 15)" (1 line)               -- BROKEN

my @range = 0..2;
.say for @numbers[@range];      # raku: 4 / 8 / 15 (3 lines)
                                 # mutsu: "(4 8 15)" (1 line)               -- BROKEN
```

## Minimal repro

```raku
my @numbers = <4 8 15 16 23 42>;
my $range := 0..2;
.say for @numbers[$range];
```

- `raku`: `4`, `8`, `15` (three separate lines)
- `mutsu` (`target/debug/mutsu`): `(4 8 15)` (one line — the slice stayed a single list item)

## Affected files (starting point)

Array/List postcircumfix `[...]` subscript compilation — likely
`vm/vm_var_ops.rs` or the index/subscript compiler helpers. The literal-Range-subscript
path apparently marks the multi-element result for flattening (e.g. wraps it in a Slip),
but the variable-subscript path does not carry that same "subscript was a
multi-value/Range/Array selector" flag through to the `for` loop's iteration source. Needs
the two paths to converge on the same flattening behavior regardless of whether the
subscript expression is a literal or a variable.
