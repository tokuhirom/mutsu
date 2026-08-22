# `is foo[...]` custom variable-trait argument sugar is misparsed as a parameterized-type trait

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Sub.rakudoc:78`).

## Root cause

`parse_variable_traits` (`src/parser/stmt/decl/my_decl.rs`) handles a trait name followed
by `[...]` unconditionally as "parameterized type trait" sugar for `is Set[Int]`-style
type traits (lines ~551-581): it folds the entire bracket content into the `trait_name`
string itself (`trait_name = format!("{}[{}]", trait_name, param_text)`), regardless of
whether the trait name is a real uppercase type (`Set`, `Foo`) or a lowercase
custom-trait-call name.

Real Raku also supports `is TraitName[...]` as sugar for calling a **lowercase**
`trait_mod:<is>` candidate with a single Array-literal argument — analogous to the
already-supported `is TraitName<a b>` sugar (an adjacent `<...>` word-list, handled a few
lines below in the same function, line ~597) which desugars to `is TraitName(<a b>)`. When
the trait name is a lowercase custom trait, `[1,2,3,...]` should be parsed as an
array-literal expression argument (same as the explicit `is foo([1,2,3,...])` parenthesized
form), not folded into the type name string.

Because the current code always folds it into the name, the VM's trait dispatch later
receives a bogus trait name like `"foo[1,2,3,:named<a>, :2b, :3c]"` (the entire bracket
content, verbatim) and no argument at all, so it reports "Unknown variable trait" even
when a matching `trait_mod:<is>` candidate for `foo` exists.

## Minimal repro

```raku
multi trait_mod:<is>(Variable $a, :@foo) {
    say "called with @foo[]"
}
my $x is foo[1,2,3] = 1;
```

- `raku`: runs the custom trait, printing `called with 1 2 3`.
- `mutsu` (`target/debug/mutsu`): `X::Comp::Trait::Unknown: Unknown variable trait
  'is foo[1,2,3]'`.

The doc's fuller example (`Type/Sub.rakudoc:78`) additionally destructures the array
literal's positional/named parts via a complex signature
(`:@foo [$firstpos, *@restpos, :$named, *%restnameds]`), but the crash reproduces on the
much simpler case above — the destructuring signature is a separate, secondary concern
once the basic bracket-argument parse is fixed.

## Affected files (starting point)

- `src/parser/stmt/decl/my_decl.rs` (`parse_variable_traits`, the `r2.starts_with('[')`
  branch around lines 551-581) — needs to distinguish an uppercase type-parameterization
  trait (fold into the name, current behavior) from a lowercase custom-trait bracket
  argument (parse as an Array-literal expression argument, mirroring the `(...)` and
  `<...>` branches immediately below it).
