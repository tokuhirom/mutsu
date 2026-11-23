use Test;
use experimental :rakuast;

plan 6;

# A tree that `EVAL` runs keeps every argument of `take` / `fail`, and the
# declarator docs of an enum, a subset, an anonymous sub and a parameter, as
# the parser does for the same source (#16356).

is-deeply EVAL(Q[(gather take(1, 2)).List].AST), ((1, 2),), 'take(1, 2) takes one list';
is-deeply EVAL(Q[(gather take-rw(1, 2)).List].AST), ((1, 2),), 'so does take-rw';
is EVAL(Q[sub g { fail("a", "b") }; g().exception.message].AST), 'ab',
    'fail("a", "b") joins its arguments';

is EVAL(Q:to/END/.AST), 'Enumeration|small ints', 'enum and subset docs';
#| Enumeration
enum Colors < Red Green Blue >;
#| small ints
subset Small of Int where * < 10;
Colors.WHY ~ '|' ~ Small.WHY
END

is EVAL(Q:to/END/.AST), 'doc', 'a parameter doc';
sub a2($a = 1, #= doc
) {}
&a2.signature.params[0].WHY
END

throws-like { EVAL Q[sub f {}; temp &f.wrap({ 1 })].AST }, X::Localizer::NoContainer,
    'temp over a call on a code variable';
