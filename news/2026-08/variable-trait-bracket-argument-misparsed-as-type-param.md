# `is foo[1,2,3]` variable-trait argument sugar is no longer folded into the trait name

```raku
multi trait_mod:<is>(Variable $a, :@foo) { say "called with @foo[]" }
my $x is foo[1,2,3] = 1;
```

prints `called with 1 2 3` in rakudo; mutsu raised
`X::Comp::Trait::Unknown: Unknown variable trait 'is foo[1,2,3]'`.

## Root cause

`parse_variable_traits` (`src/parser/stmt/decl/my_decl.rs`) treated a trait name
followed by `[...]` unconditionally as `is Set[Int]`-style **type
parameterization**, folding the whole bracket content into the trait name string
(`trait_name = format!("{}[{}]", trait_name, param_text)`). Trait dispatch then
received the bogus name `foo[1,2,3]` and no argument at all.

Raku also spells a *custom trait* call with a single Array-literal argument that
way: `is foo[1,2,3]` is `is foo([1,2,3])`, exactly analogous to the already
supported `is foo<a b>` word-list sugar handled a few lines below. Only an
uppercase name — a real type — parameterizes.

## Fix

The bracket branch now splits on the `is_uppercase_start` flag the function
already computes: an uppercase name keeps the fold-into-the-name behaviour, and a
lowercase name parses `[...]` with `container::array_literal` and passes the
result as the trait argument, joining the existing `(...)` and `<...>` branches.

`is foo[1,2,3]`, `is foo[1,2,:named<a>]` and the empty `is foo[]` now all match
rakudo, and `is Array[Int]` still parameterizes. Pinned by
`t/custom-operator-and-term-parsing.t` section 6.
