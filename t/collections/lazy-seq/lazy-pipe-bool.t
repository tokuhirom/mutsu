use Test;

plan 16;

# The truth of a lazy `.map`/`.grep` pipe is whether the STAGE yields a first
# element, not whether its source has one (#16939): `Bool`/`so`/`not`, the
# prefix `?`/`!` and a boolean context all pull exactly one result.
sub fresh { Seq.new((1, 2, 3).iterator) }

is fresh().grep({ False }).Bool, False, 'grep over Seq.new($iterator), no result: Bool';
is fresh().grep(* == 2).Bool, True, 'grep over Seq.new($iterator), a result: Bool';
is fresh().grep({ False }).so, False, 'so';
is fresh().grep({ False }).not, True, 'not';
is (?fresh().grep({ False })), False, 'prefix ?';
is (!fresh().grep({ False })), True, 'prefix !';
is fresh().map(* * 2).grep(* == 9).not, True, 'a grep chained on a map';

is (1, 2, 3).lazy.grep({ False }).Bool, False, 'lazy list grep, no result';
is (1..5).lazy.grep(* > 2).Bool, True, 'lazy range grep, a result';
is (1..5).lazy.grep(* > 9).Bool, False, 'lazy range grep, no result';
is (1, 2, 3).lazy.map(* * 2).so, True, 'lazy map, so';
is (1, 2, 3).lazy.map(* * 2).not, False, 'lazy map, not';

my $held = (1, 2, 3).lazy.grep({ False });
is $held.Bool, False, 'a pipe held in a variable';
is ($held ?? 'yes' !! 'no'), 'no', 'a pipe in a ternary';

# Only one result is pulled; the rest stays lazy.
my $pulled = 0;
my $seq = (1..Inf).lazy.map({ $pulled++; $_ });
ok $seq.Bool, 'an infinite pipe is true';
is $pulled, 1, 'exactly one element was pulled';
