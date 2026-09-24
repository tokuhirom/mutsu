use v6;
use nqp;
use Test;

# The object-level nqp:: ops are the VM's own routines (ADR-0118 §2.3):
# `nqp::istrue` is the boolification `?`/`if` use, `nqp::istype` is the type
# check `~~` makes, `nqp::isconcrete` asks concreteness (not `.defined`),
# `nqp::eqaddr` asks object identity (not `.WHICH`), and `nqp::clone` makes a
# real copy. Expected values were measured with rakudo.

plan 21;

class False-Bool { method Bool { False } }
is nqp::istrue(False-Bool.new), 0, 'istrue honours a user Bool (it answered 1)';
is nqp::istrue(False-Bool.new), +?False-Bool.new, '... as ? does';
is nqp::istrue((1, 2, 3).grep(* > 5)), 0, 'istrue of an empty .grep result';
is nqp::istrue("0"), 1, 'istrue "0" is 1, as ?"0" is True';
is nqp::istrue(0), 0, 'istrue 0';

class M is Mu { }
is nqp::istype(M, Any), 0, 'istype of an `is Mu` class against Any (it answered 1)';
is nqp::istype(M, Any), +(M ~~ Any), '... as ~~ does';
is nqp::istype(3, Int), 1, 'istype 3, Int';
is nqp::istype("3", Numeric), 0, 'istype "3", Numeric';
enum Colour <Red Green>;
is nqp::istype(Red, Colour), 1, 'istype of an enum value against its enum';

is nqp::isconcrete(Failure.new("x")), 1, 'a Failure is concrete (it answered 0)';
is nqp::isconcrete(Empty), 1, 'Empty is concrete';
is nqp::isconcrete(Int), 0, 'a type object is not';
is nqp::defined(Failure.new("y")), 1, 'nqp::defined asks the same question';

class W { method WHICH { ObjAt.new("same") } }
my ($a, $b) = W.new, W.new;
is nqp::eqaddr($a, $b), 0, 'eqaddr ignores a user WHICH';
ok $a === $b, '... which === honours';
is nqp::eqaddr($a, $b), 0, '... and a === in between does not change eqaddr';
is nqp::eqaddr($a, $a), 1, 'eqaddr of the same object';

class P { has $.x is rw }
my $o = P.new(x => 1);
my $c = nqp::clone($o);
nqp::bindattr($c, P, '$!x', 5);
is $o.x, 1, 'binding an attribute of an nqp::clone leaves the original alone (it was the same object)';
is nqp::eqaddr($o, $c), 0, '... into a different object';
my @arr = 1, 2;
isa-ok nqp::clone(@arr), Array, 'nqp::clone of an Array is an Array (it was a List)';
