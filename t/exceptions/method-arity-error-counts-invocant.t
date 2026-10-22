use Test;

# A method's "Too many/few positionals" counts the invocant (`self`) in both
# numbers, as Rakudo does (#14565). Expected values are the `raku` output.

plan 8;

class P {
    method m($x) { }
    method n() { }
    method o($x, $y) { }
    submethod s($z) { }
    method w($x, :$k) { }
}
sub msg(&code) { try { code(); CATCH { default { return .message.lines[0] } } }; 'no error' }

is msg({ P.new.m(1, 2) }), 'Too many positionals passed; expected 2 arguments but got 3', 'one parameter, one surplus';
is msg({ P.new.n(1) }), 'Too many positionals passed; expected 1 argument but got 2', 'no parameters: singular';
is msg({ P.new.o(1, 2, 3) }), 'Too many positionals passed; expected 3 arguments but got 4', 'two parameters';
is msg({ P.new.s(1, 2) }), 'Too many positionals passed; expected 2 arguments but got 3', 'a submethod counts it too';
is msg({ P.new.w(1, 2) }), 'Too many positionals passed; expected 2 arguments but got 3', 'named parameters do not count';
is msg({ P.m(1, 2) }), 'Too many positionals passed; expected 2 arguments but got 3', 'called on the type object';

role R { method r($a) { } }
class RC does R { }
is msg({ RC.new.r(1, 2) }), 'Too many positionals passed; expected 2 arguments but got 3', 'a role method';

sub f($a) { }
is msg({ f(|(1, 2)) }), 'Too many positionals passed; expected 1 argument but got 2', 'a sub does not count an invocant';
