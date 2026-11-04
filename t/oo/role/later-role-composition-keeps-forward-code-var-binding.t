use v6;
use Test;

plan 6;

# A class that composes a role AFTER a run-time statement is a BEGIN-time
# effect, so the unit's prologue declares the earlier `my &f := ...` names
# ahead of the statements. A closure that calls a `&name` bound LATER must
# still see that later binding, not the declaration's placeholder (#16351).

role R { }
class Id1 does R { }

my &a1 := -> Id1 $a { b1 $a }
my &b1 := -> Id1 $a { "b1 ok" }

is a1(Id1.new), 'b1 ok', 'a closure calls a later-bound `my &b1 :=`';

class Id2 does R { }

is a1(Id2.new.WHAT === Id2 ?? Id1.new !! Id1.new), 'b1 ok', 'and again after the later class is composed';

# A binding that precedes the closure is still captured as before.
my &early := -> { 'early' }
my &user := -> { early() }
is user(), 'early', 'a closure over an already-bound `&early`';

# Reassigning a bound name is seen through the by-name call.
my &c1 := -> { c2() }
my &c2 := -> { 'c2 first' }
is c1(), 'c2 first', 'forward call through two later bindings';

# The same shape with a plain `=` assignment.
my &d1 = -> { d2() }
my &d2 = -> { 'd2 ok' }
is d1(), 'd2 ok', 'assignment form';

class Id3 does R { }
is Id3.new ~~ R, True, 'the role is composed by the late classes';
