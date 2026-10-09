# Text::Emoji loads and passes: expression-position container traits, trans `$/`, Regex AT-KEY subscripts

Text::Emoji's `t/01-basic.rakutest` now passes 21/21. Three general fixes:

- `do my %h is Cls = ...` (and so `BEGIN my %h is Cls = ...`) applied the container trait twice, the
  second time STOREing the instance into itself (`X::Hash::Store::OddNumber` for Hash::Agnostic classes).
- The closure of a `Regex => Callable` `.trans` rule now sees the match as `$/`.
- `$obj{/re/}` on an instance with a user `AT-KEY` dispatches to a `Regex:D` candidate (Map::Match).
