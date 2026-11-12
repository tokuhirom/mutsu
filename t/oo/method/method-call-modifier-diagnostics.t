use Test;

# `.+name` / `.*name` on the topic call every candidate along the MRO, as on an
# explicit invocant; a modifier followed by something that cannot start a method
# name is a compile-time error with rakudo's diagnosis (found running
# Polyglot::Brainfuck, whose `{++.+++}` used to die at run time).

plan 8;

class A { method m { 'A' } }
class B is A { method m { 'B' } }

given B.new {
    is .+m.join(','), 'B,A', '.+name on the topic calls every candidate';
    is .*m.join(','), 'B,A', '.*name on the topic calls every candidate';
    is .*nope.elems, 0, '.*name with no candidate is empty';
}

sub diag-of(Str $code) {
    try EVAL $code;
    $!.^name ~ ': ' ~ $!.message
}

is diag-of('say 1.*(1)'), 'X::Comp::AdHoc: Cannot use .* on a non-identifier method call',
    '.* before a paren';
is diag-of('say 1.?(1)'), 'X::Comp::AdHoc: Cannot use .? on a non-identifier method call',
    '.? before a paren';
is diag-of('say .+(1)'), 'X::Comp::AdHoc: Cannot use .+ on a non-identifier method call',
    '.+ before a paren on the topic';
is diag-of('say 1.+ Str'), 'X::Syntax::Malformed: Malformed postfix call',
    'whitespace after the modifier';
is diag-of('say 1.?3'), 'X::Syntax::Malformed: Malformed postfix call',
    'a digit after the modifier';
