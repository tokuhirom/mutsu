# Pod::Contents 0.1.1 goes from partial to green

Locked and worked via the ecosystem distribution roulette (board:
[#7884](https://github.com/tokuhirom/mutsu/issues/7884)). `Pod::Contents`
0.1.1 went from partial (1/2 baseline files, 1/51 assertions) to green (2/2
files, 51/51 assertions).

The distribution exercises several ordinary Raku features that mutsu handled
in isolation but not in combination:

- variable traits such as `my @items is List:D` and `is List:U` now keep the
  definedness smiley attached to the type constraint without changing the
  trait name;
- `samewith .contents` and the related redispatch primitives accept listop
  arguments, so a topic method call is parsed and forwarded correctly;
- a bare capture in a subsignature accepts named arguments that the inner
  signature does not name; and
- `map`/`grep` invoke `.assuming` callables through the normal VM binding path,
  avoiding a second application of their primed arguments.

The fixes are general interpreter behavior, not a native replacement for the
distribution. They are pinned by `t/oo/trait/list-trait-smiley.t` and
`t/routines/dispatch/samewith-assuming-grep.t`.
