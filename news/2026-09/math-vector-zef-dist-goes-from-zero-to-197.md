# Math::Vector: a zef distribution from 0 of 201 assertions to 197

`Math::Vector` 0.6.0 loaded under mutsu but its only test file died on line 3 of the suite, so the
ledger recorded `status: red` with `mutsu_assertions: 0` against a rakudo baseline of 201. Five
general interpreter bugs stood between those two numbers. None of them is about vectors.

## `->` after a term whose parser ate its own trailing whitespace

```raku
for flat (1, 2) X (3, 4) -> $x, $y { ... }
```

threw `Unsupported use of -> as postfix. In Raku please use: either . to call a method, or whitespace
to delimit a pointy block` — a diagnosis that states its own refutation. Raku's rule here is purely
lexical: `->` **glued** to a term is the obsolete Perl 5 arrow, and whitespace before it delimits a
pointy block. Normally the remainder carries that distinction, because with a space the remainder
starts with the space rather than with `->`. But a bare listop whose argument list is extended with a
list-infix operator (`flat (...) X (...)`) consumes the whitespace itself, so by the time the postfix
loop looked, the arrow was flush against the term and the obsolete-syntax check fired on a perfectly
good pointy block.

`postfix_expr_loop_from` already tracks exactly the fact needed to tell those apart —
`term_ends_with_ws`, the consumed span's own answer to "did I end on whitespace", which
`brace_is_postcircumfix` relies on for the same class of term parsers. Gating the arrow check on it
fixes the whole family at once, and `$x->foo` is still rejected.

## An operator whose name contains `/` was reaped from the registry by its own slash

`multi infix:</>(Math::Vector $a, $b) is export` vanished the moment its module finished loading, so
`$vector / $scalar` fell through to numeric division and threw `Cannot resolve caller
Numeric(Math::Vector:D: )`. Every sibling operator in the same file — `+ - * % **` — worked.

The post-load pass in `runtime_module.rs` that reaps *non-exported* operator `GLOBAL::` routines
derived each key's operator name by splitting at the **first** `/`. For `GLOBAL::infix:</>/2` that is
the operator's own slash, yielding `infix:<`, which no entry in the exported-operator set can
match — so an exported candidate was classified as non-exported and deleted. The correct extraction
already existed a module away: `function_key_base_name` in `dispatch_resolve.rs` takes the arity
suffix at the rightmost `/` *followed by an ASCII digit*, precisely so an operator's own slash
survives. It is now split into a reusable `function_key_strip_arity_suffix` and used at the reap
sites, plus in the `MY::`/`UNIT::` pseudo-stash listing, which had been spelling the operator
`&infix:<` for the same reason.

## `×` and `÷` lost their core implementation, and `×=` looked up the wrong name

rakudo makes the Unicode spellings aliases of the ASCII routines — `&infix:<×> === &infix:<*>` is
`True`, and `&infix:<÷>.name` is `infix:</>` — while keeping them separate *names* for user
declarations. Two consequences, and mutsu had both backwards:

- A user `multi infix:<×>` sits in front of the shared core implementation rather than replacing it.
  Math::Vector's `×` is constrained to three dimensions (`where { $a.dim == 3 }`), so `$v5 × $v6` on
  5-vectors must reach the core candidate and throw on the numeric coercion. mutsu diverts `×` off
  `OpCode::Mul` as soon as any user `infix:<×>` exists, and the generic `call_infix_fallback`
  reduction that then answered runs the *lenient* `builtins::arith_*`, which read an object as 0 —
  turning a `dies-ok` into a silent `Int`. `×`/`÷` now fall back to the same strict numeric coercion
  their ASCII opcodes apply.
- `×=` is the assignment metaop over the name **as spelled**, so it reaches a user `infix:<×>` where
  `*=` (a different name) does not. mutsu mapped `×=` to `CompoundAssignOp::Mul` unconditionally,
  desugaring it to `*=`. It now declines the alias when a user `infix:<×>` is declared, exactly as
  `parse_multiplicative_op` already does for the bare operator, which hands the spelling to the
  generic user-symbol-infix metaop path that was already there for `⋅=`.

## `.perl` did not reach a user `raku` override

`Mu.perl` is rakudo's deprecated spelling of `.raku` and is implemented by *calling* `self.raku`, so
a class that overrides only `raku` renders through that override under either name. mutsu resolved
the two names independently and fell through to its own attribute-dump renderer, which broke
Math::Vector's `multi method raku()` — it renders nested vectors with `@.components.map({ .perl })`,
and the result is asserted to round-trip through `EVAL`. `.perl` now delegates when the class has a
user `raku` and no `perl` of its own; the gate keeps `callsame` from inside an explicit `perl`
override reaching the default as before.

## A Range subscript on a `does Positional` instance is a slice

`$vec[1..2]` answered `Nil` while `$vec[0, 2]` worked: only the comma-list index arm knew how to
slice an instance. A Range names its indices outright, so — unlike the Whatever arms beside it, which
need `.elems` and are deliberately narrow — it needs nothing from the class beyond the `AT-POS` the
single-index arm already calls, and now applies to every `does Positional` class. An unbounded end
(`$vec[1..*]`) would need `.elems` and is still left alone.

## Result

`Math::Vector` 0.6.0: **0 → 197 of 201** baseline assertions, `t/01-basics.rakutest` no longer dies
mid-file.

Three neighbouring ledger records, picked by root cause rather than convenience, moved for free —
which is the evidence that these are interpreter fixes and not a distribution's special case:

| distribution | before | after |
| --- | --- | --- |
| `octans` 0.2.5 | `blocked_load`, 0 assertions | **`green`**, 11/11, 2/2 files |
| `ABC` 0.6.13 | `partial`, 320/904; `t/10-utils.t` a `regression` at 0/119 | `partial`, **429**/904; `t/10-utils.t` now `partial` at 109/119 |
| `Color` 1.004001 | `t/06-operators.rakutest` a `regression`, dying on the first statement | that file now reaches its plan as a `partial` |

`octans` and `ABC`'s `t/10-utils.t` were both the `->`-after-a-listop parse gap; `Color`'s operator
file was the `Numeric` coercion reached through a user infix. The four remaining assertions are two filed gaps, neither of them about this distribution:
[#8006](https://github.com/tokuhirom/mutsu/issues/8006) (a user `multi infix:<cross>` must shadow the
core `cross` rather than extend it — the classification of which core infixes are `proto`/`multi` and
which are plain `sub`s) accounts for three, and
[#8007](https://github.com/tokuhirom/mutsu/issues/8007) (a compound assignment inside a block skips
the declared type check when the base op dispatches to a user infix) accounts for the last.
[#8008](https://github.com/tokuhirom/mutsu/issues/8008) was found along the way: a module's own body
cannot reach the operator multis it exports, which is independent of the `/` mangling above and
affects `*` equally.

Pins: `t/lang/operators/metaop-cross-listop-arg-pointy-block.t`,
`t/modules/import-export/module-export-slash-operator.t`,
`t/lang/operators/user-infix-unicode-times-slash.t`,
`t/oo/method/methods-instance-perl-raku-delegation.t`,
`t/collections/subscript/positional-instance-range-slice.t`.
