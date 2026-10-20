use Test;

# A role body runs once per composing class, and a `sub` the body declares
# closes over that run's lexicals: each class gets its own `$count`, and it
# persists across calls (#13957).

plan 7;

role R { my $count = 10; method m { f() }; method c { $count }; sub f { ++$count } }
class K does R { }
class L does R { }

is K.new.m, 11, 'the body sub increments the class\'s counter';
is K.new.m, 12, 'the increment persists across calls';
is K.new.c, 12, 'a method of the role sees the sub\'s write';
is L.new.m, 11, 'another composing class has its own counter';

role S { my $n = 0; method inc { ++$n } }
class A does S { }
class B does S { }
is A.new.inc ~ A.new.inc ~ B.new.inc, '121', 'role-body state through methods only';

role T { my @log; sub note($x) { @log.push: $x }; method add($x) { note($x); @log.elems } }
class X does T { }
X.new.add('a');
is X.new.add('b'), 2, 'a body array written through a body sub';

role U { my $hits = 0; sub bump { $hits++ }; method go { bump(); $hits } }
class W does U { sub other { 'mine' }; method own { other() } }
is W.new.go ~ W.new.go ~ W.new.own, '12mine', 'a class body sub of another name stays';
