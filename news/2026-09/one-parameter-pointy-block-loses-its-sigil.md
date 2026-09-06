# A one-parameter pointy block lost its parameter's sigil, so `@`/`%` arguments bound by value

Two independent tickets filed while taking `Template6` 0.16.0 from 0/12 to 10/12
are closed here. Both turned out to be information the *parser* threw away, not
anything the VM did wrong.

## `-> @stack { … }` did not bind the caller's container

`todo/tickets/array-arg-mutation-lost-on-the-second-call-through-a-slurpy-relay.md`
described a mutation that reached the caller the first time a relay sub was used
and was silently lost on every call after that. The "second call" framing was a
symptom of the reduction, not the invariant. Re-measuring the neighbourhood
found something much simpler and much wider:

```raku
my $push = -> @stack { @stack.unshift('for') };
my @s; $push(@s);
say @s.raku;    # raku: ["for"]    mutsu: []
```

A pointy block with **exactly one** parameter lost every `@`/`%` mutation, on
the very first call, through every shape that holds a callable (a `$` variable,
an `&`-sigil variable, a hash element, an array element). A block with **two**
parameters was already correct, which is why the ticket's `push` handler
(`-> @stack, $a`) worked while its `pop` handler (`-> @stack`) did not, and why
the failure looked like a warm-up effect.

The root cause is in `src/parser/primary/misc/lambda.rs`. A single-parameter
pointy block with no traits takes a "simple" route that builds an
`Expr::Lambda`, which carries only a **sigil-stripped name** and no `ParamDef`
at all — `-> @x` and `-> $x` both become `param: "x"`. That compiles to
`MakeLambda`, whose arguments go through `bind_function_args_values`'s
defs-less legacy branch; with no sigil and no `ParamDef` that branch cannot
tell a container parameter from a scalar one, so it binds by value and every
`.push` / `.unshift` / `.shift` / element write inside the block was dropped.

The file already excluded `@_`/`%_` from that route for exactly this reason
("stripping leaves the name `_`"). The fix generalizes that exclusion to every
`@`/`%` parameter, so a one-parameter container block takes the same
`AnonSubParams` path a two-parameter one always took. Everything else about
such a block is unchanged: it is still a `Block`, still has arity 1, and its
`.signature` now correctly keeps the sigil — and, as a bonus, it now rejects a
non-`Positional` argument the way raku does.

One consequence had to be fixed with it. `runtime/sequence.rs` used the `@`/`%`
sigil in a generator's `params` as a proxy for "slurpy", collecting the whole
history window into it. That proxy was only ever safe because a plain `-> @row`
used to arrive sigil-stripped; with the sigil restored, `$[1], -> @row { … } ... *`
started slurping. Slurpiness is now read from `ParamDef::slurpy` /
`double_slurpy` whenever the signature carries defs, with the sigil heuristic
kept only for the genuinely defs-less legacy shapes (placeholders,
`WhateverCode`). `-> *@history` still slurps; `-> @row` binds one previous
element, as raku does.

`Template6` 0.16.0 goes from 10/12 to **11/12** test files: `t/02-for.rakutest`
now passes. The one remaining file is the separately-filed
`todo/tickets/template6-include-local-data-not-reaching-the-included-stash.md`.

Pinned by `t/pointy-block-container-param.t`, whose output is byte-identical
under `raku` and `mutsu`.

## A trailing comma in an attribute default dropped the declaration

`todo/tickets/trailing-comma-in-attribute-default-drops-the-declaration.md`:

```raku
class C { has @.a = 1, 2,; method m { @!a } }
say C.new.m.raku;    # raku: [1, 2]   mutsu: []
class D { has %.t = a => 1, b => 2,; }
say D.new.t.raku;    # raku: {:a(1), :b(2)}   mutsu: No such method 't'
```

`has_decl.rs` hand-rolled the `@`/`%` initializer's comma split instead of
using the shared splitter in `parser::stmt::assign::comma`, and so missed both
of that splitter's rules. It consumed a comma and then *required* another
expression, so a trailing comma failed the whole `has` parse — the statement
fell back to `BareWord("has")` plus a plain assignment, which is why the
`%`-sigil case lost its generated accessor too, not merely its value. It also
never re-lifted the operators that are looser than the comma, and never skipped
whitespace before the comma. Three measured divergences, one cause:

| | before | raku |
| --- | --- | --- |
| `has @.a = 1, 2,;` | `[]` (and no accessor) | `[1, 2]` |
| `has @.a = 1 , 2;` | `[1]` | `[1, 2]` |
| `has @.j = 1, 2 ... 6;` | `[1, (2…6).Seq]` | one merged sequence |

The loop now calls the shared `comma_list_ends_here` and
`normalize_comma_list_items`, and keeps the one-element-plus-trailing-comma
slot unflattened (`has @.a = 1..5,` is `[1..5,]`, not `[1, 2, 3, 4, 5]`).
Pinned by `t/attr-default-trailing-comma.t`, also byte-identical against raku.
