# Multi-parameter `for` loops declare their parameters

A pointy block with several parameters (`for @l.kv -> $i, $child { ... }`) used to
bind them with plain assignments. Inside a sub or method, where the names had no
local slot, that assignment resolved to whatever `child` meant in the enclosing
scope — so a caller's `my $child := @a[0]` had its element overwritten, and in a
method the parameter read back as `Nil`. The `Math::Symbolic` distribution hit this
through `Tree.find_all`.

The parameters are now declarations of the loop block: each gets its own local slot
in a scope frame that encloses the body, a same-named parameter of a nested loop
shadows the outer one with a fresh slot, and a name that had no slot before the loop
goes back to by-name resolution afterwards (#9689).

Binding the parameters as real declarations surfaced a stringification bug that the
old assignment path had hidden: a type object held in a variable's container inside a
list (`my $T = Any; (11, $T).Str`) stringified as its gist, `11 (Any)`, instead of the
empty string. `Str` context now looks through the container, so it is `11 ` as in raku.
