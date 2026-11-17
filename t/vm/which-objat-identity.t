use Test;

# Every `.WHICH` call returns a fresh ObjAt: two of them are `===` (same
# WHICH text) but not `=:=` (different objects), #17599.

plan 10;

my class A { }
my $one = 1;

nok A.WHICH =:= A.WHICH, 'two .WHICH calls on a type object are distinct objects';
nok $one.WHICH =:= $one.WHICH, '... and on a value';
ok  A.WHICH === A.WHICH, '... while === still compares the WHICH text';

my $w = A.WHICH;
ok  $w =:= $w, 'one ObjAt is =:= itself';
my $v := $w;
ok  $v =:= $w, '... and a binding of it';
ok  $w !=:= A.WHICH, '!=:= against a fresh one';
nok ([=:=] A.WHICH, A.WHICH), 'the [=:=] reduction agrees';

my @l = $w, $w;
nok @l[0] =:= @l[1], 'two array elements are distinct containers';
ok  @l[0]<> =:= @l[1]<>, '... holding the same ObjAt';
ok  @l[0] === $w, '=== against the element';
