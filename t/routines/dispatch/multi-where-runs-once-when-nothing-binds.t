# When no user candidate binds, `resolve_function_with_types` retries with
# wider candidate sets (optional-arity, slurpy, any-arity), and each set
# re-gathers the exact-arity candidates it already tried. Their `where`
# clauses ran again on every retry -- three times per `2 * 3` against a
# declined user `infix:<*>`, where rakudo runs it once. Found through the
# Bitcoin distribution, whose `secp256k1` guards `infix:<*>` candidates with
# `where 1 < $n < 2**256`, so every integer multiplication paid for it three
# times over.
use Test;

plan 6;

my $runs = 0;
multi infix:<*>(Int $n where { $runs++; False }, Int $m) { 'user' }
my ($a, $b) = 2, 3;
is $a * $b, 6, 'a declined user operator falls back to the core one';
is $runs, 1, 'and its where clause ran once';

$runs = 0;
multi f(Int $n where { $runs++; False }) { 'user' }
multi f(Int $n, $extra?) { 'optional' }
is f(1), 'optional', 'a wider optional-arity candidate still wins';
is $runs, 1, 'without re-running the rejected candidate\'s where clause';

$runs = 0;
multi g(Int $n where { $runs++; $n > 5 }) { 'big' }
multi g(*@rest) { 'slurpy' }
is g(1), 'slurpy', 'a slurpy fallback still wins';
is $runs, 1, 'and the rejected where clause ran once';
