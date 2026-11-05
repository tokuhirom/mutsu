use Test;

plan 27;

nok 5 ~~ int, 'a boxed Int is not a native int';
nok 5 ~~ uint8, 'a boxed Int is not a native uint8';
nok 1e0 ~~ num64, 'a boxed Num is not a native num64';
nok "x" ~~ str, 'a boxed Str is not a native str';
ok 5 !~~ int, 'negated smartmatch recognizes a boxed Int';
my $boxed = 5;
nok $boxed ~~ int, 'a boxed Int read from an untyped scalar stays boxed';

my int $int = 5;
nok $int ~~ int, 'a natively declared int variable is read boxed (RakuAST)';
ok $int !~~ int, 'negated smartmatch sees the boxed read';
nok $int ~~ int8, 'a native int does not match int8';
ok $int ~~ Int, 'a native int still matches boxed supertype Int';
ok $int ~~ Numeric, 'a native int still does Numeric';

my int8 $int8 = 5;
nok $int8 ~~ int8, 'a natively declared int8 variable is read boxed';
nok $int8 ~~ int, 'a native int8 does not match int';
nok $int8 ~~ int8:_, 'the representation-neutral smiley sees the boxed read too';
nok $int8 ~~ int8:D, 'native int8 rejects the definite-object smiley';
nok $int8 ~~ int8:U, 'native int8 rejects the type-object smiley';

my num64 $num64 = 1e0;
nok $num64 ~~ num64, 'a natively declared num64 variable is read boxed';
nok $num64 ~~ num32, 'a native num64 does not match num32';
ok $num64 ~~ Num, 'a native num64 still matches boxed supertype Num';

my str $str = "x";
nok $str ~~ str, 'a natively declared str variable is read boxed';
ok $str ~~ Str, 'a native str still matches boxed supertype Str';

nok (my int $decl = 5) ~~ int, 'a declaration expression boxes its result';
nok ($int + 0) ~~ int, 'arithmetic boxes its result';
nok ($_ ~~ int) given $int, 'topicalization boxes a native value';

sub accepts-int(int $value) { $value ~~ int }
ok accepts-int(5), 'a direct native parameter read retains its declared type';

my int ($p, $q) = 1, 2;
ok $p ~~ int, 'an element of a destructured native declaration keeps the native type';
my int $plain;
nok $plain ~~ int, 'a native declaration without an initializer is read boxed';
