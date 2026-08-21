# `is TraitName<word list>` trait-argument sugar is not parsed

## Symptom

Raku's `is TraitName<a b c>` sugar — a bareword word-list immediately after a
trait name, meaning "pass this list as the trait's argument" (sugar for `is
TraitName(<a b c>)`, which itself becomes a `:TraitName<a b c>` named arg to
`trait_mod:<is>`) — is not recognized by mutsu's parser at all. The `<a b c>`
is instead parsed as an unrelated, separate word-list expression statement:

```raku
multi sub trait_mod:<is>(Variable:D \v, :@restricted!) is export {
    say "called with restricted = {@restricted.raku}";
}
my %h2 is restricted<a b> = a => 42, b => 666;
say "done: {%h2.raku}";
```

mutsu:
```
X::Comp::Trait::Unknown: Unknown variable trait 'is restricted'
```
(and, on a bare declaration with no initializer, a "Useless use of constant
string ... in sink context" warning for each word — confirming `<a b>` is
being parsed as a discarded standalone expression, not a trait argument).

raku:
```
called with restricted = ("a", "b")
done: {:a(42), :b(666)}
```

## Root cause (partial diagnosis)

`is restricted` (no argument attached) dispatches with `has_arg = false`
(`trait_value = Value::TRUE`, `vm/vm_var_trait_ops.rs`'s
`exec_apply_var_trait_op`), so the argument-list form of the trait
(`:@restricted!`) never even gets a chance to match — mutsu's parser never
attaches `<a b>` as the trait's argument expression at all. This is a parser
gap (wherever `is TraitName` trait syntax is parsed — likely in
`src/parser/`, the declarator/trait parsing path), not a runtime dispatch
issue.

## Discovered via

Investigating `todo/deep/trait-mod-does-not-callable-sub.md` (`trait_mod:<does>`
callable-sub support). `Hash::Restricted`'s `is restricted<a b>` form (the
`restrict-given[%allowed]` parametric-role branch) hits this gap; its
`multi sub trait_mod:<is>(Variable:D \v, :@restricted!)` candidate never
receives the intended array argument, so it either silently mis-dispatches
(falls through to the OTHER `Bool:D :$restricted!` candidate with
`$restricted = True`, since `has_arg` reads false) or hits the "Unknown
variable trait" error above when only the array-form candidate is declared.
This blocks 8 of `Hash::Restricted`'s 32 subtests (the `%h2 is
restricted<a b>` half of `t/01-basic.rakutest`); the `%h1 is restricted`
(bareword, no `<...>`) half is unaffected and works once `trait_mod:<does>`
itself is callable (see `news/2026-08/trait-mod-does-callable-sub.md`).

## Repro

See the snippet above (`tmp/repro5.raku`/`tmp/repro6.raku` in the
investigating session — not checked in).

## Next steps

Find where `is TraitName` trait syntax is parsed (declarator trait list,
likely near variable/routine declaration parsing in `src/parser/`) and add
support for a `<...>` word-list immediately following the trait name, folding
it into the trait's argument the same way `is TraitName(...)` already does.
Verify against `raku -e` what the argument shape is for other trait forms too
(`is TraitName(...)` with a single bareword, a Pair, etc.) to make sure the
fix generalizes rather than special-casing just the word-list case.
