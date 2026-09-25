# Literal attribute names resolve once per TRIR site

Another part of ADR-0121 D3 landed (#9291): `nqp::getattr` / `nqp::bindattr`
sites in a TRIR routine whose attribute name is a string literal, which is how
nqp code almost always writes them (`nqp::getattr($o, IB, '$!a')`).

## What such a site paid

Before this change the site compiled to `NqpOpGen`, the generic boxed dispatch.
On every execution it:

- walked the interpreter-coupled op table's string `match` to find `getattr`;
- turned the name operand back into a string and stripped its twigil;
- probed the attribute store with a string key;
- and, when the class operand was a user class, resolved that bareword through
  the whole term-resolution chain. That was the largest part: about 4,700
  instructions of a 6,740-instruction `getattr`.

## What changed

- **`GetAttrC` / `BindAttrC`.** A literal name is interned and twigil-stripped
  when the chunk is compiled (`NqpAttrName`, `src/runtime/nqp_attr.rs`). A
  plain instance is answered straight from its attribute store. Every other
  receiver (a List's `$!reified`, a Map's `$!storage`, a Match, a lazy Match)
  takes the generic body with the name as written, so the two paths cannot
  disagree. The typed forms' conversions (`_i` / `_n` / `_s`) moved into one
  place, `NqpAttrConv`, which the generic ops now use too.
- **`ClassOperand`.** The attribute ops ignore the class operand's value, since
  an instance has one attribute store, not one per class. What the operand
  still owes is its effect: it has to resolve. A bareword class operand now
  remembers the type object it resolved to, keyed on the registry write
  generation, so it resolves again only after a declaration changes.
- **Bareword resolution.** Independently of TRIR, `push_bare_word_value` asked
  "is this an imported routine alias?" by `format!`-ing and interning a
  `Pkg::name` key for every enclosing package, before it knew whether the name
  was a type at all. It now asks that only when anything was imported, through
  the memoized `qualified()` pairs. The package-scoped enum probe reads the
  package as a symbol instead of cloning its name twice.

## Measured

Release build, ns per op inside a TRIR routine (`tmp/nqp-bench.raku` from
ADR-0121 §6, operands passed as `$` scalars so the routine is linked), paired
runs in one session. rakudo 2026.07 is shown for reference.

| op | before | after | rakudo |
|---|---:|---:|---:|
| `getattr($o, IB, '$!a')` | ~662 | ~105 | ~46 |
| `bindattr($o, IB, '$!b', $i)` | ~670 | ~110 | ~48–158 |
| `getattr($r, List, '$!reified')` | ~150 | ~97 | ~46 |
| `getattr($h, Map, '$!storage')` | ~145 | ~95 | ~46 |
| `bindattr($r, List, '$!reified', $b)` | ~290 | ~190 | ~30 |

In callgrind, one iteration of the `getattr($o, IB, '$!a')` loop went from
6,740 to about 1,000 instructions (the empty loop is about 260).

## Found on the way

The same benchmark passed its receivers as `@r` / `%h`, and a TRIR call site
with an `@`/`%` argument for a `$` parameter never links, so those rows were
measuring the untyped path (~1,000 ns). The bench now passes `$` scalars. The
unlinked `@`-argument call is a separate TRIR gap.
