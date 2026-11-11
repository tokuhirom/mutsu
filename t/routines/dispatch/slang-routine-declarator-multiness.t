use lib 't/lib';
use Test;

# A slang `routine-declarator` after a `multi` / `proto` / `only` declarator
# (#16888, EEL 0.3's `multi pattern` / `proto pattern`): rakudo's
# `multi-declarator` parses `<declarator>` after the keyword, so the slang's
# production is tried there, and the method its `method-def` reads takes the
# multiness (`$*MULTINESS`) the keyword set.

plan 8;

use PatternDeclaratorSlang;

class Greeter {
    multi shout greet(Int $x) { number }
    multi shout greet(Str $x) { word }
    only shout single { alone }
}

is Greeter.greet(1), 'NUMBER!', 'a multi candidate by its signature';
is Greeter.greet('a'), 'WORD!', 'the other multi candidate';
is Greeter.single, 'ALONE!', 'an only declarator';
ok Greeter.^find_method('greet').is_dispatcher, 'the multi candidates share a dispatcher';
is Greeter.^find_method('greet').candidates.elems, 2, 'two candidates';
is Greeter.^find_method('greet').candidates.map(*.tag).sort.join(','),
    'greet/1/number,greet/1/word', 'each candidate is the method its action saw';

class Proto {
    proto shout pick(|) {*}
    multi shout pick(Int $x) { int }
}

is Proto.pick(3), 'INT!', 'a proto declared by the slang dispatches to its multi';
ok Proto.^find_method('pick').is_dispatcher, 'the proto is the dispatcher';
