use Test;

# `my Mu $x := EXPR` compiles as `my $x := EXPR`: every value binds to
# `Mu`, so the declared type is unobservable after the bind.

plan 10;

my $x := 1; $x := Mu;
is $x.raku, 'Mu', 'an untyped scalar rebinds to Mu';

my Mu $y := 2; $y := Mu;
is $y.raku, 'Mu', 'a Mu-typed scalar rebinds to Mu';

my Mu $z := 3;
is $z.VAR.^name, 'Int', '.VAR of a value-bound Mu scalar is the value';

my Mu $q := my $ = 5;
is $q.VAR.^name, 'Scalar', 'a bind to a container aliases it';
$q = 6;
is $q, 6, 'assigning through the bound container';

my Mu $p := 1;
throws-like { $p = 2 }, X::AdHoc, message => 'Cannot assign to an immutable value',
    'a Mu-typed value bind is readonly';

class A { method m { my Mu $v := self; $v.^name } }
is A.new.m, 'A', 'inside a method';

my Mu $u := Mu;
is $u.raku, 'Mu', 'a bind to Mu itself';

my @a = 1, 2;
my Mu $r := @a;
$r.push(3);
is @a.join(','), '1,2,3', 'a Mu-typed bind to an array aliases it';

my Mu $t;
is $t.raku, 'Mu', 'a Mu-typed declaration without a bind still defaults to Mu';

