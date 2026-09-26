# Comparisons, bitwise ops, gcd/lcm and reductions: a non-numeric Str is a Failure

The arithmetic operators already evaluated to a lazy `X::Str::Numeric` Failure
when a Str operand could not be numified. The rest of the numeric operator family
still took a wrong-answer path, numifying the bad string to `0`: `"a" < 2` was
`True`, `"a" +| 1` was `1`, `"a" gcd 2` was `2` and `[+] "a", 1` was `1`
([#9573](https://github.com/tokuhirom/mutsu/issues/9573)).

They now match rakudo:

- `==`, `<`, `<=`, `>`, `>=` (and `≤`/`≥`) check for a non-numeric Str operand in
  their shared operator bodies (`num_eq_values` etc.), so the opcode, the
  `&infix:<...>` routine form, reductions and hyper/zip metaops all agree. `!=`
  stays a `Bool` (`True`), since it is `not ==` over a falsy Failure, and a chained
  reduction `[<] "a", 1` is `False`, as in rakudo.
- `+&`, `+|`, `+^`, `+<`, `+>`, `gcd` and `lcm` check in their one
  `builtins::arith` primitive. The reduction table's separate copies of `gcd` and
  `lcm` now call that primitive too.
- The arithmetic reduction/metaop leaf (`[+]`, `»+«`, `Z*`, ...) checks in the same
  way as the arithmetic opcodes.

`==` used to be kept lenient on purpose, because mutsu modeled `PromiseStatus` as
bare strings and `$p.status == Kept` compared `"Kept"` with `"Kept"`.
`PromiseStatus` is now a real built-in enum (`Planned` 0, `Kept` 1, `Broken` 2),
registered like `Order` and `SeekType`. `Promise.status` answers the enum value, so
it compares numerically, smartmatches the enum and `.raku`s as
`PromiseStatus::Kept`.
