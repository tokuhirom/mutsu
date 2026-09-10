use Test;

plan 25;

nok 5 ~~ int, 'a boxed Int is not a native int';
nok 5 ~~ uint8, 'a boxed Int is not a native uint8';
nok 1e0 ~~ num64, 'a boxed Num is not a native num64';
nok "x" ~~ str, 'a boxed Str is not a native str';
ok 5 !~~ int, 'negated smartmatch recognizes a boxed Int';
my $boxed = 5;
nok $boxed ~~ int, 'a boxed Int read from an untyped scalar stays boxed';

my int $int = 5;
ok $int ~~ int, 'a direct native int read matches int';
nok $int !~~ int, 'negated smartmatch recognizes a native int';
nok $int ~~ int8, 'a native int does not match int8';
ok $int ~~ Int, 'a native int still matches boxed supertype Int';
ok $int ~~ Numeric, 'a native int still does Numeric';

my int8 $int8 = 5;
ok $int8 ~~ int8, 'a direct native int8 read matches int8';
nok $int8 ~~ int, 'a native int8 does not match int';
ok $int8 ~~ int8:_, 'native int8 accepts the representation-neutral smiley';
nok $int8 ~~ int8:D, 'native int8 rejects the definite-object smiley';
nok $int8 ~~ int8:U, 'native int8 rejects the type-object smiley';

my num64 $num64 = 1e0;
ok $num64 ~~ num64, 'a direct native num64 read matches num64';
nok $num64 ~~ num32, 'a native num64 does not match num32';
ok $num64 ~~ Num, 'a native num64 still matches boxed supertype Num';

my str $str = "x";
ok $str ~~ str, 'a direct native str read matches str';
ok $str ~~ Str, 'a native str still matches boxed supertype Str';

nok (my int $decl = 5) ~~ int, 'a declaration expression boxes its result';
nok ($int + 0) ~~ int, 'arithmetic boxes its result';
nok ($_ ~~ int) given $int, 'topicalization boxes a native value';

sub accepts-int(int $value) { $value ~~ int }
ok accepts-int(5), 'a direct native parameter read retains its declared type';
