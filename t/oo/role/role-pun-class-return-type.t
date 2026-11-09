use Test;

plan 7;

# A role method written `--> ::?CLASS` run on a pun (the role itself) means
# the role; composed into a class it means that class.
role R {
    method a(--> ::?CLASS) { self }
    method d(--> ::?CLASS) { 5 }
    method r(--> ::?ROLE) { self }
}

is R.new.a.^name, 'R', 'a pun answers its invocant through --> ::?CLASS';
is R.new.r.^name, 'R', 'and through --> ::?ROLE';
my $got = (try R.new.d);
ok !$got.defined, 'a wrong value fails the return check';
like $!.message.lines[0], /'Type check failed for return value; expected R but got Int (5)'/,
    'the message names the role';
is R.a.^name, 'R', 'the type object too';

class C does R { }
is C.new.a.^name, 'C', 'composed into a class it is the class';
ok !(try C.new.d).defined, 'and checked against the class';

