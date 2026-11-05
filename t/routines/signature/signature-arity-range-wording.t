use Test;

# Rakudo words the arity of "Too few/many positionals passed" by the signature's
# shape: N argument(s), "N or M", "N to M", or "at least N ... only G" when a
# slurpy lifts the upper bound.

plan 14;

sub msg(&c, |args) { try c(|args); $!.message }

sub a1($x, $y?, $z?, $w?) { }
sub a2($x, $y, $z?, $w?) { }
sub a3($x, $y?, *@r) { }
sub a4($x, $y) { }
sub a5($x) { }
sub a6($x, $y?) { }
sub a7($x, $y?, $z?) { }
sub a8($x, *@r) { }
sub b1($x, $y = 5) { }
sub b5($x?) { }

is msg(&a1), 'Too few positionals passed; expected 1 to 4 arguments but got 0', 'optional tail: a range';
is msg(&a2, 1), 'Too few positionals passed; expected 2 to 4 arguments but got 1', 'two required, two optional';
is msg(&a3), 'Too few positionals passed; expected at least 1 arguments but got only 0', 'optional and a slurpy';
is msg(&a4), 'Too few positionals passed; expected 2 arguments but got 0', 'all required';
is msg(&a5), 'Too few positionals passed; expected 1 argument but got 0', 'one required';
is msg(&a6, 1, 2, 3), 'Too many positionals passed; expected 1 or 2 arguments but got 3', 'too many, one optional';
is msg(&a7, 1, 2, 3, 4), 'Too many positionals passed; expected 1 to 3 arguments but got 4', 'too many, two optional';
is msg(&a8), 'Too few positionals passed; expected at least 1 arguments but got only 0', 'a slurpy';
is msg(&b1), 'Too few positionals passed; expected 1 or 2 arguments but got 0', 'a default makes it optional';
is msg(&b5, 1, 2, 3), 'Too many positionals passed; expected 0 or 1 arguments but got 3', 'only an optional';

my $l = -> $a, *@r { };
is msg($l), 'Too few positionals passed; expected at least 1 arguments but got only 0', 'pointy block with a slurpy';

class K { method m($x, *@r) { }; method n($x, $y?) { } }
my $k = K.new;
is (try $k.m()) // $!.message, 'Too few positionals passed; expected at least 2 arguments but got only 1', 'a method counts its invocant';
is (try $k.n(1, 2, 3)) // $!.message, 'Too many positionals passed; expected 2 or 3 arguments but got 4', 'a method, too many';

sub limit(Str:D $s, Int $limit) { }
try limit Str, 3;
is $!.message,
    "Parameter '\$s' of routine 'limit' must be an object instance of type\n'Str', not a type object of type 'Str'. Did you forget a '.new'?",
    'a concreteness error is word-wrapped at 72 columns';
