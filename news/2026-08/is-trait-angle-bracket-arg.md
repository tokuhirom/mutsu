# `is TraitName<word list>` trait-argument sugar

Raku has a sugared form for attaching a word-list argument to a variable
trait: `is TraitName<a b c>` is shorthand for `is TraitName(<a b c>)`, which
itself desugars to a `:TraitName<a b c>` named argument passed to
`trait_mod:<is>`. mutsu's parser never recognized this form at all — the
`<a b c>` was parsed as a completely separate, unrelated word-list
expression statement, so `is restricted<a b>` dispatched `is restricted`
with *no* argument and the intended `:@restricted!` candidate of
`trait_mod:<is>` never matched:

```raku
multi sub trait_mod:<is>(Variable:D \v, :@restricted!) is export {
    say "called with restricted = {@restricted.raku}";
}
my %h2 is restricted<a b> = a => 42, b => 666;
```

mutsu raised `X::Comp::Trait::Unknown: Unknown variable trait 'is
restricted'` (or, without a matching bareword candidate, silently
mis-dispatched to one), while raku ran the intended candidate and printed
`called with restricted = ("a", "b")`.

## The fix

`src/parser/stmt/decl/my_decl.rs` parses `is TraitName` trait lists for
variable declarators in two loops (traits can alternate with `of Type`
constraints). Both already handled `is TraitName(...)` by parsing an
`expression()` between a matched pair of parens and attaching it as the
trait's argument. The fix adds a sibling branch: if the trait name is
*immediately* followed by `<` (no intervening whitespace — matching
Rakudo, which rejects `is TraitName <a b>` with a space as a parse error),
the `<...>` word-list is parsed with the same angle-word-list parser used
for ordinary term position (`crate::parser::primary::angle_list`, newly
exposed as `pub(crate)` for this reuse) and attached as the trait's
argument, exactly like the parenthesized form.

The adjacency check works by capturing the parser's cursor right after the
trait-name identifier is parsed, before any whitespace-skipping happens —
the same idiom already used for ordinary no-whitespace postcircumfix
`(`/`[`/`{` elsewhere in the parser.

No new AST or runtime changes were needed: word-list literals already
desugar at parse time to the right shape (a single word like `<a>` becomes
a bare `Str` literal, multiple words like `<a b>` become an `Expr::ArrayLiteral`),
matching raku's behavior (`<a>.WHAT` is `Str`, `<a b>.WHAT` is `List`)
exactly. Once the argument expression reaches `custom_traits`, the existing
declaration-trait compilation and `trait_mod:<is>` dispatch machinery
(unchanged) works without modification.

Pinned by `t/is-trait-angle-bracket-arg.t`, covering the multi-word case,
the single-word case, and a regression check that plain `is TraitName`
(bareword, no argument) and `is TraitName(...)` (parenthesized) still parse
correctly.

## Discovered via

Found while investigating `Hash::Restricted`, whose `restrict-given[%allowed]`
parametric-role branch uses `is restricted<a b>`; this blocked 8 of its 32
`t/01-basic.rakutest` subtests before this fix.
