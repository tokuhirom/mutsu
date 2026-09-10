use v6;
use Test;

plan 7;

# `ok(...) or <expr>` — with Test loaded, `ok(...)` parses via the known-call
# statement path, which must not swallow the trailing low-precedence infix
# (URI's november-urlencoded.rakutest `ok(...) or say ...` shape).
my $ran = False;
ok(1 == 1, 'passing assertion') or $ran = True;
nok $ran, 'or-branch does not run when the assertion passes';

todo 'intentional failure to drive the or-branch';
ok(1 == 2, 'failing assertion') or $ran = True;
ok $ran, 'or-branch runs when the assertion fails';

# `and` continuation.
my $and-ran = False;
ok(1 == 1, 'passing assertion for and') and $and-ran = True;
ok $and-ran, 'and-branch runs when the assertion passes';

# The statement-modifier forms still parse.
my $n = 0;
ok(1 == 1, 'modifier if') if $n == 0;
