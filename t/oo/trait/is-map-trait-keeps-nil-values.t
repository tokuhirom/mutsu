use Test;

# `my %m is Map = ...` builds the Map from the initializer, as `Map.new` does:
# its values are bare, so a `Nil` value stays `Nil` rather than becoming the
# `Any` a Hash container holds (#12478). Checked against rakudo.

plan 9;

my %pairs is Map = (b => Nil, c => 3);
ok %pairs<b> =:= Nil, 'a Nil value in a pair list stays Nil';
is %pairs<c>, 3, 'other values are kept';

my %seq is Map = <x y>.map({ $_ => Nil });
is %seq.raku, 'Map.new((:x(Nil),:y(Nil)))', 'a Seq initializer keeps its Nil values';

my $runs = 0;
my %once is Map = <p q>.map({ $runs++; $_ => 1 });
is $runs, 2, 'the initializer runs once';

my %items is Map = a => (1, 2);
is %items<a>.raku, '(1, 2)', 'a list value is held bare';

# A single Pair initializer, literal or held in a variable (#17932).
my %single is Map = a => Nil;
ok %single<a> =:= Nil, 'a single Pair initializer keeps its Nil value';
my $pair = a => Nil;
my %from-var is Map = $pair;
ok %from-var<a> =:= Nil, '... and so does a Pair held in a variable';
my %mixed is Map = a => Nil, b => 2;
ok %mixed<a> =:= Nil, 'a comma-separated pair list keeps it too';

dies-ok { my %ro is Map = a => 1; %ro<a> = 2 }, 'the Map stays immutable';

done-testing;
