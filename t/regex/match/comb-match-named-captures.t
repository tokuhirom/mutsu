use Test;

# `.comb($regex, :match)` returns whole Match objects, named and positional
# captures included. It used to build bare position-only Matches, so
# `$<var>:exists` was always False and `$<path>` was Nil (Path::Map's
# `for $path.comb($componentrx, :match).list -> $/ { ... }`).

plan 8;

my $rx = /
  [ <?after '/'> | ^^ ] [ $<slurpy> = '*' | [ $<var> = ':' ]? $<path> = <-[/*]>+ ]
  /;
my @m = 'date/:year/:month'.comb($rx, :match);

is @m.elems, 3, 'three matches';
is-deeply @m.map(~*).List, <date :year :month>, 'their text';
is-deeply @m.map({ ~$_<path> }).List, <date year month>, 'a named capture';
is-deeply @m.map({ $_<var>:exists }).List, (False, True, True), 'an optional capture';
is-deeply @m.map(*.from).List, (0, 5, 11), 'their positions';

is ~'a1b22'.comb(/(\d+)/, :match)[1][0], '22', 'a positional capture';
is-deeply 'xyz'.comb(/<[a..z]>/, :match, 2).map(~*).List, <x y>, 'with a limit';
is-deeply 'a1b2'.comb(/\d/).List, ('1', '2'), 'without :match it is still the strings';
