# A `subset` declared in a package is named `Package::Name`, and `throws-like` accepts a Regex matcher

Two independent gaps, both surfaced by the last failing assertion in the
upstream `HTTP::UserAgent` test suite, which is now green end to end (27/27).

## `subset` package qualification

A `subset` declared in the body of a `class` / `module` / `role` belongs to that
package, so raku names it `Package::Name` — that is what `.^name` reports and
what every type-check message prints. mutsu treated the short name as the
subset's identity: `register_subset_decl` added a `Pkg::Name` alias to the
registry and to `env`, but the bare `env` entry still pointed at the *bare*
symbol, so `RM.^name` answered `RM` where raku answers `Foo::RM`.

The fix follows the shape `class` / `role` registration already uses: the
qualified name becomes the canonical identity and the short name is an alias
pointing at it. Both keys stay in `registry().subsets`, because most constraint
lookups are by the exact name written at the use site.

The other half is the attribute type constraint. `has RM $.method` records the
name as written, and that string is what `X::TypeCheck::Assignment` prints. It
cannot be qualified at record time — a `subset` in a class body is registered
when the body *runs*, after `has` has already been recorded — so the resolution
moved to the read side, where the declaring class is known:
`get_attr_type_constraint` and `collect_attribute_type_constraints` now route
the recorded name through the existing `resolve_type_name_for_owner`. That
resolves the short name against the owning class's package chain, so an
inherited attribute is reported qualified by the class that *declared* it
(`expected Base::Pos`), matching raku. `resolve_type_name_for_owner` gained an
early-out for core type names so the common `has Int $.n` case does not allocate
an `Owner::Int` probe on every `.new`.

A `my subset` stays lexical and gets no qualified alias, and a mainline subset
keeps its bare name.

## `throws-like` with a Regex matcher

`Test`'s `throws-like` smart-matches the thrown exception against its second
argument (`$_ ~~ $ex_type` in `Test.rakumod`), so a Regex is a legal matcher and
is checked against the exception's stringification. mutsu stringified that
argument up front and only ever compared it as a *type name*, so a Regex matcher
failed against every exception — including a plain `die "abc def"` tested with
`/'abc def'/`. The matcher value is now kept, and a Regex (with or without
adverbs) routes through the same `matcher_accepts` helper the named `.attr =>`
matchers already used.

Pins: `t/subset-package-qualified-name.t` (21 assertions, each checked against
raku), `t/throws-like-regex-matcher.t`.

Two neighbouring divergences found during the work were recorded rather than
fixed here, since both need their own roast sweep:
`todo/tickets/type-check-assignment-message-format.md` (the typed-`my` path
prints `expected X, got Y` instead of raku's `expected X but got Y (repr)`) and
`todo/tickets/accessor-assignment-error-names-dot-attr.md` (an `is rw` accessor
assignment names `$.attr` where raku names `$!attr`).
