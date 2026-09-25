# List methods and `Z`/`X`/`xx` stream over infinite input; the hard caps are gone

Issue [#9159](https://github.com/tokuhirom/mutsu/issues/9159). Lazy iteration used to exist only for
`map` and `grep` over an infinite source. Every other list method was an eager `Vec` transform, and a
few operators hid this behind fixed prefix caps:

| Expression | before | now (and raku) |
| --- | --- | --- |
| `(1..*).map(* + 1).skip(2).head(2)` | `()` | `(4 5)` |
| `(1..*).map(-> $a, $b { $a + $b }).head(3)` | dies "Not enough elements" | `(3 7 11)` |
| `(1..*).map(* + 1).rotor(2).head(2)` (also `batch`, `unique`, `repeated`, `squish`, `flat`, `produce`) | dies "Cannot .rotor a lazy list" | `((2 3) (4 5))` |
| `((1..*) Z (1..*))[1500]`, `zip((1..*), (1..*))[1500]` | `Nil` (1000-row cap) | `(1501 1501)` |
| `((1..*) X (1, 2))[600]` | `Nil` (256-element cap) | `(301 1)` |
| `roundrobin((1..*), (5, 6)).head(3)` | panic: capacity overflow | `((1 5) (2 6) (3))` |
| `(42 xx *)[10**5]` | `Nil` (4096-element cap, 256 for a thunk) | `42` |
| `gather { take 1; take 2; take 3 }.skip(1)` | `()` | `(2 3)` |

## The mechanism

The fix is a general pull-iterator protocol rather than eight point fixes. A lazy `map`/`grep` stage
(`MapGrepSpec`) can now carry a `PipeAdaptor`, a stateful adaptor that pulls from its source(s) one
step at a time and appends what it emits to the pipe's cache. The variants are `Skip`, `MultiMap`,
`Chunk` (`rotor`/`batch`), `Distinct` (`unique`/`repeated`/`squish`), `Produce`, `Zip`, `Cross`
(odometer order, so an infinite later operand never lets an earlier one advance, as in Rakudo),
`Roundrobin` and `Repeat` (`xx`). Each of them works the same over a finite reified source and an
infinite one, so no prefix cap is needed anywhere. The pull side is in `src/vm/vm_helpers_lazy_adaptor.rs`
and the construction side is in `src/vm/vm_helpers_lazy_adaptor_build.rs`.

- Method calls: `try_lazy_adaptor_method` runs at all three method-dispatch entries, before the
  forcing block. It turns `.skip`/`.rotor`/`.batch`/`.unique`/`.repeated`/`.squish`/`.produce`/`.flat`
  on an infinite Range or a genuinely-lazy list into a stage. Argument shapes that only the eager
  implementation understands (`rotor(*)`, a Callable `skip`) still go to that implementation.
- A multi-arity `.map` block gets a `MultiMap` stage instead of making `make_lazy_pipe` decline.
- `Z` and `zip()`: when every operand is unbounded, the result is a lazy `Zip` stage, and it is finite
  as soon as any operand is. When some operand is finite, `zip_rows_bounded` pulls each unbounded
  operand only as far as the finite one reaches. `X`/`cross()`: any unbounded operand gives a lazy
  `Cross` stage. `ZipIter` and `MAX_ZIP_EXPAND` are deleted.
- `xx`: the VM opcode, the runtime operator fallback and the `[xx]` reduction now share one
  `Interpreter::list_repeat`. The three copies had already drifted: only the VM flattened a callable's
  Slip result, and `[xx]` had a different eager limit.
- Single homes: `distinct_admit` holds the per-element `unique`/`repeated`/`squish` rule and the eager
  methods use it too. `zip_with_combine_row` holds the `zip(:with)` fold.

Two other hidden caps of the same kind went with these:

- `rotor(1..*)` pre-expanded its count cycle to 10,000 entries. The next count is now computed.
- Forcing a lazy pipe over a *provably finite* source stopped at 1,000,000 elements and then raised
  X::Cannot::Lazy. It now runs to the source's end.

Some consumers had been reading the old fixed prefix, so they now pull exactly what they need:

- `polymod` with an infinite divisor list pulls as many divisors as the invocant has bits.
- A hyper operator whose sole dwim side is infinite pulls that side to the finite side's length.

Pinned by `t/collections/lazy-seq/lazy-list-adaptors-stream.t`. Every expectation in that file was
checked against `raku`.
