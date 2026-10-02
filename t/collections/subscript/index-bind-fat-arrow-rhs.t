use Test;

# From the String::Color distribution: `=>` and `:=` share the item-assignment
# tier and associate right, so `%h<k> := $x => 1` binds the Pair.
plan 5;

my %c;
my $x = 5;
%c<k> := $x => 1;
is-deeply %c<k>, (5 => 1), 'indexed bind takes the whole pair';

my %s;
my $color := %s<a> := "red";
%c<j> := $color => 2;
is-deeply %c<j>.key, "red", 'key of the bound pair is the plain value';
with %c<j> { %s<b> := .key }
is-deeply %s, %{a => "red", b => "red"}, 'no bind marker leaks into a hash value';

my @a;
@a[0] := "q" => 3;
is-deeply @a[0], ("q" => 3), 'positional indexed bind takes the pair';

my %h;
%h<z> := "k" => "v";
is-deeply %h<z>, ("k" => "v"), 'literal key pair';

done-testing;
