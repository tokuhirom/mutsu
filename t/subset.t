use Test;
plan 7;

subset PositiveInt of Int where $_ > 0;

sub grab(PositiveInt $x) { return $x }

is grab(1), 1, 'subset accepts positive int';
dies-ok { grab(0) }, 'subset rejects zero';
dies-ok { grab(-1) }, 'subset rejects negative';

subset Bug::RT80930 of Int where { !.defined || $_ %% 2 };
lives-ok { my Bug::RT80930 $rt80930 }, 'subset with :: in the name is declared';

my subset S-Int of Int;
my subset S-Str of Str;
ok S-Int.isa(Int), 'subset isa base type';
nok S-Int.isa(S-Str), 'subset isa unrelated subset is false';

my subset MyInt of Int where { True };
my MyInt $x = 5;
$x = Nil;
# Under the default (6.d) language version, assigning Nil to a subset-typed
# scalar resets it to the *subset* type object (not the nominal base). The 6.e
# nominalization-to-base behavior is covered by roast/S02-types/subset-6e.t.
ok $x === MyInt, 'assigning Nil to subset-typed scalar resets to the subset type object';
