use v6;
use Test;

plan 4;

# A `start` block whose placeholder Raku rejects is lowered where the parser
# built it (ADR-15760 slice 2) and still reports the placeholder diagnostic.

my $code = Q[my $p = start { $^a }; 1];
try EVAL($code);
is $!.^name, 'X::Placeholder::Block', 'start { $^a } is a placeholder block error';

try EVAL(Q[start { my $x = 1; $^b + $x }]);
is $!.^name, 'X::Placeholder::Block', 'a placeholder after a declaration is rejected too';

# A start block with no rejected placeholder keeps running.
is (await start { 6 * 7 }), 42, 'a plain start block runs';
is (await start { $_ = 5; $_ + 1 }), 6, 'a start block topic is its own';
