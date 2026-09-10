use Test;
plan 10;

# Anonymous enum with < >
my $e = enum < foo bar baz >;
is $e.keys.elems, 3, 'anonymous enum has 3 keys';
is $e<foo>, 0, 'anonymous enum first value is 0';
is $e<bar>, 1, 'anonymous enum second value is 1';
is $e<baz>, 2, 'anonymous enum third value is 2';
isa-ok $e, Map, 'anonymous enum isa Map';

# Anonymous enum with ::
my %e2 = enum :: < x y z >;
is +%e2<y>, 1, 'enum :: form works';

# anon keyword
anon enum < alpha >;
is +alpha, 0, 'anon enum keyword works';

# Single value
my %e3 = enum <single>;
is %e3.keys.elems, 1, 'single-value enum has 1 key';
is %e3<single>, 0, 'single-value enum value is 0';

# Prefix +/- on hash variables
my %h = "a" => 42;
is +%h<a>, 42, 'prefix + on hash angle bracket access';
