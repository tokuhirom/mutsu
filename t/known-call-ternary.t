use Test;

plan 6;

is do { defined(42) ?? 1 !! 2 }, 1, 'ternary after known call parses in do block';

sub optional($p?) { defined($p) ?? $p !! 'undef' }
is optional('abc'), 'abc', 'ternary after known call parses in sub body';
is optional(), 'undef', 'optional arg + ternary else branch works';

# A naked listop term (e.g. `done`) in the then-branch would gobble the `!!`,
# so Rakudo requires parenthesizing it. The parens must let it parse and the
# else-branch must still be reachable.
sub picks-else { my $r = 'init'; 0 ?? (done) !! ($r = 'else'); $r }
is picks-else(), 'else', 'parenthesized `(done)` in then-branch keeps ternary intact';

# A parenthesized control-flow term in the then-branch fires when selected.
sub picks-then { for 1..5 { $_ == 2 ?? (last) !! Nil }; 'returned' }
is picks-then(), 'returned', 'parenthesized `(last)` in then-branch parses and runs';

# The explicit call form `done()` has always parsed; keep it covered.
sub call-form { my $r = 'init'; 0 ?? done() !! ($r = 'else'); $r }
is call-form(), 'else', 'ternary then-branch `done()` call parses';
