# A malformed operator name says what is wrong with it

Two `sub <category>:<…>` spellings rakudo diagnoses precisely, and mutsu
answered with the generic failure it falls back to when a name does not parse:

| source | rakudo | mutsu (before) |
| --- | --- | --- |
| `sub infix:[/./] { 42 }` | `X::Syntax::Extension::TooComplex` — "Colon pair value '/./' too complex to use in name" | `Missing block` |
| `sub meow:<bar> { }` | `X::Syntax::Extension::Category` — "Cannot add tokens of category 'meow'" | `Missing block` |

Same shape as the parse-diagnosis work already landed this month
(`news/2026-08/parse-error-keeps-its-exception-class.md`,
`news/2026-08/three-parse-failures-keep-their-malformed-class.md`): the parser
had recognised enough to know the construct was an operator declaration, but
`parse_sub_name_inner` simply declined to consume the colon pair and returned the
bare base name, so the leftover `:[…]` / `:<…>` made the *block* look missing and
that is what got reported.

`operator_name_extension_error` now inspects exactly that leftover. Both the
`parse_sub_name` path and the `sub`-declaration path (which calls
`parse_sub_name_inner` directly, to build its own richer null-operator group
error) consult it.

## The ordering the ticket did not have

Measured against rakudo with
`raku -e 'use MONKEY-SEE-NO-EVAL; try EVAL @*ARGS[0]; say $!.^name' '<code>'`,
the colon-pair check runs **before** the category check:

```
sub meow:[bar] { }   ->  X::Syntax::Extension::TooComplex   # not Category,
sub meow:<bar> { }   ->  X::Syntax::Extension::Category     # though meow is
```

so `meow:[bar]` is TooComplex even though `meow` is not a category either. The
ticket described Category as "the category before the colon is not known" and
TooComplex as a separate check, which would have produced Category there.

The same probe also shrank the set of valid categories: rakudo accepts only
`infix`, `prefix`, `postfix`, `term`, `circumfix`, `postcircumfix` and
`trait_mod`, and raises Category for `statement_control`, `statement_prefix`,
`quote`, `metaoperator` and the rest — the ticket had listed `statement_control`
among the known ones.

## `trait_auxiliary` deliberately stays accepted

rakudo raises Category for `trait_auxiliary:<is>` too — it cannot compile
`roast/S12-traits/basic.t` at all for that reason. mutsu accepts the spelling,
that file is whitelisted, and `S12-traits/parameterized.t` and
`t/imperative-does-parameterized-role.t` depend on it as well, so
`is_operator_category` keeps it. Narrowing to rakudo's set would regress three
passing files to match a file rakudo itself rejects.

## Effect

`roast/S06-operator-overloading/sub.t` is now **29/29 under the real `Test`
module** as well as the native provider. It was the file's last blocker: the
block-local operator leak went in earlier the same day
(`news/2026-08/block-local-routine-scope.md`), which took it from aborting at 24
assertions to running all 29 with these two failing.

Pin: `t/operator-extension-name-error-classes.t`. Its class assertions read
`.^name` off the caught exception rather than using `throws-like`'s type
argument — mutsu's native `throws-like` does not check that argument, so a
type-only assertion passes against the generic parse failure this fix replaces
(measured: the `throws-like` version of this pin passed 12/12 *without* the fix;
the rewritten one fails 8/14).
