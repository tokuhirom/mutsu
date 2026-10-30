use v6;
use Test;

plan 14;

# `Bool(Mu) $cond` (Test::proclaim's first parameter) stays on the light call
# path for a Bool argument; every other argument is coerced through its own
# `.Bool`, as the general binder does.
sub truthy(Bool(Mu) $cond) { $cond }
sub tagged(Bool(Mu) $cond, $desc is copy, $prefix = '') { "$prefix$desc:{ $cond.raku }" }

is truthy(True), True, 'a True argument';
is truthy(False), False, 'a False argument';
is truthy(1), True, 'an Int argument is coerced';
is truthy(0), False, 'a zero Int argument is coerced';
is truthy('a'), True, 'a Str argument is coerced';
is truthy(''), False, 'an empty Str argument is coerced';
is truthy(Int), False, 'a type object is falsey';
is truthy((1, 2)), True, 'a List argument is coerced';

class Odd { method Bool { False } }
is truthy(Odd.new), False, 'a user .Bool method is called';
class Even { method Bool { True } }
is truthy(Even.new), True, 'a user .Bool method is called (True)';

# Called in a loop with a mix of argument shapes: the call cache must not
# serve a non-Bool argument from the Bool entry.
my @got;
for True, 0, False, 'x', Odd.new, True -> $v { @got.push: truthy($v) }
is @got.join(','), 'True,False,False,True,False,True', 'mixed argument shapes through one call site';

is tagged(True, 'a'), 'a:Bool::True', 'with an is copy parameter and a default';
is tagged(5, 'a', 'p-'), 'p-a:Bool::True', 'a coerced Int with the other parameters';

# The parameter is still read-only.
sub ro(Bool(Mu) $c) { $c = False }
my $v = True;
dies-ok { ro($v) }, 'a Bool(Mu) parameter is read-only';
