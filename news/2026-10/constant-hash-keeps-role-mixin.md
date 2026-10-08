# `constant %h` keeps a role mixed into its Hash/Map

`constant %h = %hash.Map does Role` used to unwrap the mixin and store the bare Hash, so the
role's `AT-KEY`/`EXISTS-KEY` overrides never ran. The initializer is now kept as-is, matching
rakudo. Found via the `Text::Flags` distribution (`t/01-basic.rakutest` now passes 2686/2686).
