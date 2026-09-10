use v6;
use Test;

# A bare `return` in EXPRESSION position transfers control with Nil — it is
# the zero-arg &return call, not an inert bareword (Text::CSV's t/20_file.t
# guard `$error and return;` kept executing the rest of the sub).

plan 5;

sub guard ($e) { $e and return; "fell through" }
is guard(2034), Nil, 'truthy LHS: `$x and return` returns Nil';
is guard(0), "fell through", 'falsy LHS: body continues';

sub guard-or ($e) { $e or return; "ran" }
is guard-or(1), "ran", 'truthy LHS: `$x or return` continues';
is guard-or(0), Nil, 'falsy LHS: `$x or return` returns Nil';

my @out;
sub collect (*@xs) {
    for @xs -> $x {
        $x %% 2 or return;
        @out.push($x);
    }
    "end";
}
is collect(2, 4, 5, 6), Nil, 'bare return exits the whole routine from a loop';
