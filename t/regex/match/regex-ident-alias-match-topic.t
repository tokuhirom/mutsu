use Test;

# From Font::AFM: an aliased `<name=.ident>` / `<name=ident>` must resolve
# to the builtin rule, also when the topic is a Match.
plan 5;

my $m = "L a b ;" ~~ /L \s+ \S+ \s+ \S+/;
ok $m ~~ m:s/ L <succ=.ident> <lig=.ident> /, 'aliased <.ident> matches on a Match topic';
is ~$<succ>, 'a', 'first alias capture';
is ~$<lig>, 'b', 'second alias capture';

ok "abc" ~~ /<x=ident>/, 'aliased <ident> on a Str';
is ~$<x>, 'abc', 'ident spans alpha alnum*';

done-testing;
