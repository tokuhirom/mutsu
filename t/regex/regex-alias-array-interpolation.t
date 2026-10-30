use Test;

# MUGS::Games' CommandParser (`<thing=@($*ctx.items)>`): an alias over an
# array variable or a contextualized expression captures like rakudo.
plan 9;

my @w = <a b>;
my @e;

ok "b" ~~ /<x=@w>/, '<x=@w> matches';
is ~$<x>, "b", '... and captures under x';
ok "b" ~~ /<x=@(@w)>/, '<x=@(@w)> matches';
is ~$<x>, "b", '... and captures under x';
ok "b" ~~ /<x=$(~"b")>/, '<x=$(EXPR)> matches';
is ~$<x>, "b", '... and captures under x';
ok "b" ~~ /<x={@w}>/, '<x={ @w }> matches';
nok "z" ~~ /<x=@w>/, 'no element matches';
nok "z" ~~ /<x=@(@e)>/, 'an empty array never matches';
