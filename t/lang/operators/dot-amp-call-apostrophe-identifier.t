use Test;

# A topic call through a code object, `.&name`, reads the whole identifier,
# inner `'` and `-` included, as `$x.&name` does
# (CSS::Specification::Compiler: `sub lit-ws(Str:D() $_) { .&lit'r.&ws }`).

plan 4;

sub lit'r($x) { "<$x>" }
sub wrap-it($x) { "[$x]" }

is (.&lit'r given 'a'), '<a>', 'a topic .&name with an apostrophe';
is (.&lit'r.&wrap-it given 'b'), '[<b>]', 'chained after it';
sub f(Str:D() $_) { .&lit'r }
is f(3), '<3>', 'in a routine whose parameter is $_';
is ('c'.&lit'r), '<c>', 'the explicit-invocant form';
