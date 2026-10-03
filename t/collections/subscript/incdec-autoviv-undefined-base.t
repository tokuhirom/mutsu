use Test;

plan 5;

# `++`/`--` on an element of an undefined scalar autovivifies it into the
# container the subscript asks for.
my $x; $x<a>++;
is-deeply $x, ${a => 1}, '$x<a>++ makes a Hash';
my $y; $y{0}++;
is-deeply $y, ${"0" => 1}, '$y{0}++ makes a Hash keyed by the string';
my $z; $z[1]++;
is-deeply $z, $[Any, 1], '$z[1]++ makes an Array';
my $w; --$w[0];
is-deeply $w, $[-1], 'prefix -- too';
my $v; $v<a><b>++;
is-deeply $v, ${a => {b => 1}}, 'nested';
