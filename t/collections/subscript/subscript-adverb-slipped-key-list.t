use v6;
use Test;

plan 6;

my %row = a => 1, b => 2, c => 3;
my @ids = <a c>;
is-deeply %row{|@ids}:kv, %row{@ids}:kv, "a slipped key list with :kv equals the plain slice";
is-deeply %row{|@ids}:k, <a c>, ":k of a slipped key list";
is-deeply %row{|@ids}:v, (1, 3), ":v of a slipped key list";
is-deeply %row{|@ids}:p, (a => 1, c => 3), ":p of a slipped key list";

my @a = 10, 20, 30;
is-deeply @a[|(0, 2)]:kv, (0, 10, 2, 30), "an array slice of a slipped list with :kv";
is-deeply @a[|(0, 2)]:v, (10, 30), "an array slice of a slipped list with :v";
