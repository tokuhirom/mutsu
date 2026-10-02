# Multi dispatch counts required `@`/`%` positionals

The multi-candidate applicability check treated a positional parameter whose name starts with `@` or `%` as optional, so `multi f(@p, @s, UInt $c = 1, *%a)` matched `f(@pts, method => 'K')` and died with "Too few positionals" instead of falling through to the correct candidate. Found via the `Graph::RandomMaze` hexagonal maze (`Math::Nearest::nearest`).
