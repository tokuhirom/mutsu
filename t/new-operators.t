use Test;
plan 26;

# xx operator (list repetition)
is ("a" xx 3).elems, 3, 'xx creates list with correct count';
is ("a" xx 3).join(","), "a,a,a", 'xx repeats elements';
is (42 xx 0).elems, 0, 'xx with 0 gives empty list';

# Compound assignments: /= %= **=
my $a = 10;
$a /= 2;
ok $a == 5, '/= division assignment';

my $b = 17;
$b %= 5;
is $b, 2, '%= modulo assignment';

my $c = 2;
$c **= 3;
is $c, 8, '**= power assignment';

my $decl_pow **= $decl_pow;
is $decl_pow, 1, 'declaration-time **= autovivifies from neutral element';

my $decl_add += 4;
is $decl_add, 4, 'declaration-time += autovivifies and assigns';

# x= compound assignment
my $d = "ab";
$d x= 3;
is $d, "ababab", 'x= string repeat assignment';

# infix min
is (3 min 5), 3, 'infix min with ints';
is (5 min 3), 3, 'infix min reversed';
is ("b" min "a"), "a", 'infix min with strings';

# infix max
is (3 max 5), 5, 'infix max with ints';
is (5 max 3), 5, 'infix max reversed';
is ("a" max "b"), "b", 'infix max with strings';

# chained min/max
is (5 min 3 min 7), 3, 'chained min';
is (1 max 5 max 3), 5, 'chained max';

# min/max with Num
is (1.5 min 2.5), 1.5, 'infix min with Num';
is (1.5 max 2.5), 2.5, 'infix max with Num';

# andthen basic
is (42 andthen 100), 100, 'andthen with defined lhs returns rhs';
is (Nil andthen 100), Nil, 'andthen with Nil lhs returns Nil';

# orelse basic
is (Nil orelse 42), 42, 'orelse with Nil lhs returns rhs';
is (10 orelse 42), 10, 'orelse with defined lhs returns lhs';

# notandthen basic
is (Nil notandthen 42), 42, 'notandthen with Nil lhs returns rhs';
is (10 notandthen 42), Nil, 'notandthen with defined lhs returns Nil';

# chained orelse
is (Nil orelse Nil orelse 42), 42, 'chained orelse';

done-testing;
