# The `invalid-typename` ecosystem cluster, re-measured

[#7993](https://github.com/tokuhirom/mutsu/issues/7993) grouped 21 zef
distributions under one failure line — `Invalid typename 'X' in parameter
declaration.` — and asked, before any fix, how many gaps those 14 distinct
typenames really are. [#8066](https://github.com/tokuhirom/mutsu/pull/8066)
answered the first half by minimising one case per group: they do not collapse.
It fixed two of the four root causes (an exported `subset` declared in a role
body, and a role method parameter naming a sibling type relatively) and filed
[#8061](https://github.com/tokuhirom/mutsu/issues/8061) and
[#8062](https://github.com/tokuhirom/mutsu/issues/8062) for the other two.

Eight of the fourteen names were left unsettled: each sat behind a dependency a
bare checkout cannot resolve, so the ticket asked for one thing rather than more
guessing — re-run the sweep over the 21 and re-group from what it says. This is
that run, plus the three gaps it turned up that were small enough to fix in
place.

## A `subset` was not a resolvable type

`is_resolvable_type` — the question the role-method signature validator asks
about a parameter's type constraint — consulted classes, roles, enums, builtins
and native types, but never the subset registry. The gap only ever surfaced for
a **compound** name, which is why it survived this long: the same validator's
short-name fallback (`type_known_by_short_name`) does consult subsets, but it is
gated on the constraint being unqualified, and the sub pre-pass skips anything
containing `::` outright. So a subset named `A::B::C` was accepted in a sub
signature and in a class method, and rejected in a role method:

```raku
subset Lic::Year where UInt;
role Abstract { method year(Lic::Year $y) { $y } }
class Concrete does Abstract { }   # Invalid typename 'Lic::Year'
```

That is `License::Software`, a dependency of `App::Mi6` and `Ddt`:
`License/Software/Year.pm6` declares
`subset License::Software::Year where UInt | MyDateish | YearRange;` and
`unit role License::Software::Abstract`'s `multi method new` takes one.

Pinned by `t/oo/role/role-method-param-compound-subset.t`, which also keeps the
`where` clause enforced through the role method (so the name resolves to the
real subset rather than being waved through) and keeps an undeclared compound
name reported as `X::Parameter::InvalidType`.

## A lexically-scoped `my class` was not either

The same validator, the same question, a different reason. A `my class` / `my
role` registers under a **mangled** storage key (ADR-0047 P1, `Name\0<decl-id>`)
while `env` binds the bare name to it, so none of `is_resolvable_type`'s
registry probes saw it under the spelling a signature writes. Again only the
role-method path noticed: the sub pre-pass accepts such a name out of
`declared_types`, the unit's statically gathered declarations, which a role body
has no equivalent of.

```raku
my class EB { }
role R { method m(EB $x) { 1 } }   # Invalid typename 'EB'
```

`Protocol::MQTT` is exactly this: `my class EncodeBuffer { ... }` at file scope,
named by `our role Packet[...]`'s
`method !encode-body(Packet:D: EncodeBuffer $buffer --> Nil)`.
`is_resolvable_type` now follows the same alias that type-object position
follows, via `resolve_bare_type_name`, which already confirms the target is a
class or role before handing back a name. Pinned by
`t/oo/role/role-method-param-lexical-class.t`.

## `try EXPR` is a statement prefix, not a block

The third gap is not a typename gap at all — it is what `Selkie` and
`App::Moneymoor` fail on once their typename errors are out of the way:

```raku
try windows-close-handle($log-windows-handle)
    if $log-windows-handle.defined
        && $log-windows-handle.Int != INVALID-HANDLE-VALUE;
```

`try { ... }` and `gather { ... }` end their statement at the closing brace, so a
following line's `if` starts a fresh statement rather than modifying them. mutsu
decided that on the `Expr::Try` / `Expr::Gather` *variant* alone, so the
statement-prefix forms took the block path too: the modifier parser was handed
the text before the newline, declined the modifier, and the bare `if` was left to
be parsed as an `if` statement — reported as `Missing block`. `do` and `quietly`
never had the problem because they are not in that match.

The decision now also requires the consumed text to end with `}`, which is what
distinguishes the two forms. Pinned by
`t/exceptions/try-prefix-statement-modifier-next-line.t`, whose last two
assertions are the guard in the other direction: a block-form `try`/`gather`
followed by an `if` *statement* still works.

## What the re-measure says about the cluster

Re-running `scripts/ecosystem-sweep.py` over all 21 distributions with those
fixes in place, the single cluster has fragmented into unrelated root causes,
which is the answer #7993 was after. The updated records are in
`ecosystem/dists/`.

The typename failures that survive belong to tickets of their own:
[#8061](https://github.com/tokuhirom/mutsu/issues/8061) (`my role A::B` does not
install `B` into package `A` — `TAP`, `App::Mi6`, `Mi6::Helper`),
[#8115](https://github.com/tokuhirom/mutsu/issues/8115) (`Enumeration` is the one
core role that is not composable, which is what `Logic::Ternary`'s entry in the
cluster always was) and
[#8131](https://github.com/tokuhirom/mutsu/issues/8131) (an imported `constant`
type alias is not accepted as a type — `Gnome::N`'s `GType`).

The distributions that now get past their typename error land on blockers that
were never about typenames at all:
[#8062](https://github.com/tokuhirom/mutsu/issues/8062) (`class Foo is Attribute`
— the PDF family) and
[#8121](https://github.com/tokuhirom/mutsu/issues/8121) (a re-exported
`&trait_mod:<is>` is invisible to the importer — `Ddt` through `JSON::Class`).
