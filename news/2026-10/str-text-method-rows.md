# Str's text methods are rows in the built-in method table

ADR-11276's third slice moves the zero-argument text methods into the built-in
method table: `codes`, `ord`, `uc`, `lc`, `fc`, `tc`, `tclc`, `wordcase`,
`flip`, `trim`, `trim-leading`, `trim-trailing`, `chomp` and `chop`.

Rakudo declares each of them twice: on `Str`, which does the work, and on
`Cool`, which stringifies the invocant and calls `Str`'s method. The table now
has a row for each owner, and both rows point at the same handler. A plain
`Str` finds the `Str` row. An `Int`, a `Num`, a rational, a `Complex` or a
list or hash finds the `Cool` row, because `Cool` is in its MRO. The cascade
arms that still answer receivers the table cannot recognize, such as a `Bool`
or an instance of a `Cool` subclass, call the same handlers. Every method in
the family therefore has one implementation.

No answer changes. The calls get cheaper because a call on a variable is now
answered by the call-site lane in front of the `CallMethodMut` probes.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `$s.codes` | 1,376M | 332M | -75.9% |
| `$s.ord` | 1,279M | 320M | -75.0% |
| `$s.chomp` | 1,338M | 341M | -74.5% |
| `$s.trim` | 1,546M | 430M | -72.2% |
| `$s.flip` | 1,515M | 483M | -68.1% |
| `$s.uc` | 3,722M | 2,782M | -25.3% |
| `$i.flip` (Int, `Cool` row) | 1,213M | 475M | -60.8% |
| `@a.flip` (Array, `Cool` row) | 2,461M | 900M | -63.4% |
| `$i.abs` (no row) | 1,023M | 1,026M | +0.2% |
| `$s.comb` (no row) | 1,947M | 1,947M | 0.0% |
