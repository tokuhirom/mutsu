# A module proto's winner finds its own nested subs

A call through a trivial (`{*}`-only) proto takes the positional light bind
when its winner qualifies. That path ran the winner with the *caller's*
nested-sub table, so a winner defined in a module and reached through an
exported wrapper could not find its own `my sub` and died with "internal
error: frame-lexical routine 'compare' has no compiled body". It now runs the
winner with the table the winner was compiled alongside, as the general entry
already did.

`Algorithm::Diff` (`t/issue12`, `t/oo`) and `Text::Diff` now pass, matching raku.
