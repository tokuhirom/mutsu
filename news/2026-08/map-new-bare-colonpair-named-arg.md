# `Map.new`/`Hash.new` keep named arguments as data only when there is no positional

`Type/Map.rakudoc:62` teaches the distinction with a deliberately "WRONG"
example:

```raku
say Map.new("a", 1, :b(2)).keys;   # raku: (a)     mutsu: (a b)
```

A bare colonpair written directly in an argument list is a **named** argument
(ADR-0021: named-ness is a call-site property), and `Map.new`'s slurpy
positional signature does not collect it. mutsu slurped every argument,
positional and named alike, into the pair list.

## The rule, established against raku first

The existing code carried a deliberate comment saying Hash/Map must NOT strip
named args, citing roast `S02-types/hash.t` (rakudo issue #3211), which asserts
`Hash.new(:42a, :666b)` equals the positional `Hash.new((:42a, :666b))`. Both
that assertion and the doc's example are true at once; the rule that reconciles
them was derived by probing raku:

| call | raku |
|---|---|
| `Map.new(:42a, :7c)` | `(:a(42), :c(7))` — all named, kept as data |
| `Map.new("b", 3, :42a)` | `(:b(3))` — a positional is present, named dropped |
| `Map.new(:42a, "b", 3)` | `(:b(3))` — order does not matter |
| `Map.new((:42a), :7c)` | `(:a(42))` — the parenthesized pair is positional |
| `Map.new(\|(:42a), :7c)` | `(:a(42), :c(7))` — `\|` makes both named |
| `Map.new(@empty, :42a)` | `()` — an empty array is still a positional |

So: **named arguments become data only when the call carries no positional
argument at all.** `try_native_hash_construct` now splits on exactly that,
reading named-ness off the ADR-0021 `Pair`-flavour bit
(`Value::is_string_pair_value`) rather than inspecting the value's contents.

Pinned by `t/parser-expression-gaps.t`; roast `S02-types/hash.t`'s #3211
assertion is unaffected because that call is all-named.
