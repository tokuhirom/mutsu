# A `<subrule(…)>` call with arguments runs on the compiled regex engine
# (ADR-0135 Slice E): the arguments are evaluated once at the call, the callee
# resolved for those values runs as a frame, and a call that still bridges to
# the walk hands it the evaluated values instead of evaluating them again.
# These calls all used to bridge (`args`). Values verified against rakudo.
use Test;

plan 9;

grammar G1 { token TOP { <x(1)> }; token x($n) { a ** {$n} } }
is ~G1.parse("a"), 'a', 'a count argument';

grammar G2 { token TOP { <rep("ab")>+ }; token rep($s) { $s } }
is ~G2.parse("ababab"), 'ababab', 'a string argument, quantified call';

grammar G3 { token TOP { <num(3)> }; token num($d) { \d ** {$d} } }
is G3.parse("123")<num>.Str, '123', 'the callee Match keeps its name';

grammar G4 { rule TOP { <kw("let")> <ident> }; token kw($w) { $w }; token ident { \w+ } }
is ~G4.parse("let foo")<ident>, 'foo', 'an argument call in a rule';

grammar G5 { token TOP { <pair(":")> }; token pair($sep) { (\w+) $sep (\w+) } }
is G5.parse("a:b")<pair>[1].Str, 'b', 'captures inside a callee with an argument';

grammar G7 { token TOP { <ab(1)> || <ab(2)> }; token ab($n) { a ** {$n} b } }
is ~G7.parse("aab"), 'aab', 'the second branch calls with another argument';

grammar G8 { regex TOP { <x(1)> 'b' }; regex x($n) { a+ } }
is ~G8.parse("aaab"), 'aaab', 'a backtracking callee with an argument';

grammar G9 { token TOP { <c($/.from)> }; token c($at) { \w } }
is ~G9.parse("q"), 'q', 'an argument reading the caller\'s $/';

grammar G10 { token TOP { <x(2)> <x(1)> }; token x($n) { \d ** {$n} } }
is G10.parse("123")<x>».Str.join(','), '12,3', 'two calls with different arguments';
