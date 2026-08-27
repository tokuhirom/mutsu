use lib 't/lib';
use Test;
use AngleNamedArg;

plan 8;

# `:name<word>` is a quote-words value, so a number-shaped word yields the
# ALLOMORPH (IntStr / RatStr), exactly like a bare `<90>` term. This must hold
# on BOTH call-compilation paths: `f(:x<90>)` (an expression call) and the
# listop-style statement call to a routine the compiler cannot see statically
# (an imported one), which compiles through a separate argument parser.

is capture-of(:x<90>).hash<x>.^name, 'IntStr',
    ':name<90> is an IntStr through an imported routine (parenthesised)';

my $c = capture-of :x<90>;
is $c.hash<x>.^name, 'IntStr',
    ':name<90> is an IntStr through an imported routine (listop form)';

is capture-of(:x<.5>).hash<x>.^name, 'RatStr',
    ':name<.5> is a RatStr through an imported routine';

is capture-of(:x<a>).hash<x>.^name, 'Str',
    ':name<a> stays a plain Str';

is-deeply capture-of(:x<1 2>).hash<x>.map(*.^name).List, ('IntStr', 'IntStr'),
    ':name<1 2> yields a list of allomorphs';

# The allomorph matters because it is what lets the value bind to a `Numeric`
# parameter: a bare Str would fail the type check and, with several candidates,
# make the whole multi unresolvable.
is tolerant(1, 10, :abs-tol<90>), 'abs',
    'an angle-bracket named arg binds a Numeric parameter of an imported multi';
is tolerant(1, 10, :abs-tol<90>, :rel-tol<.5>), 'both',
    'two angle-bracket named args select the two-tolerance candidate';
is tolerant(1, 10), 'plain', 'the no-tolerance candidate is unaffected';
