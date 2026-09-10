use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# `return` in a non-routine block does a *non-local* return when a routine
# lexically encloses it, and throws X::ControlFlow::Return when none does.
# mutsu decided that for an EVAL'd snippet from `!routine_stack.is_empty()` --
# but a bare `{ ... }` block, a `for` body and a closure all push a
# `RoutineFrame` too (`is_block: true`). So an EVAL run from inside a mainline
# block compiled its snippet as "inside a routine", and a `return` in the
# snippet's own pointy block returned from whatever sub later called that block
# instead of throwing.

plan 6;

# The shape roast/S04-statements/return.t test 15 asserts, run from inside a
# block rather than at file scope.
{
    my $r = try EVAL 'my $d = -> $x { return 2 * $x }; sub f($x) { $d($x) }; f 42';
    nok $r.defined, 'return is lexotic: an EVALd blocks return does not target its caller';
    is $!.^name, 'X::ControlFlow::Return', 'and it is X::ControlFlow::Return';
}

# Same snippet at file scope, which already worked -- pinned so the fix cannot
# be "make both wrong the same way".
my $top = try EVAL 'my $d2 = -> $x { return 2 * $x }; sub f2($x) { $d2($x) }; f2 42';
nok $top.defined, 'and the same holds at file scope';

# A bare `return` in an EVAL from inside a block throws, and the throw is
# catchable rather than escaping to the top level.
{
    my $bare = try EVAL 'return 1';
    is $!.^name, 'X::ControlFlow::Return', 'a bare EVALd return from a block throws catchably';
    nok $bare.defined, 'and yields no value';
}

# When a REAL routine encloses the EVAL, `return` in the snippet still returns
# from it -- rakudo does this, so the fix must not turn every EVALd return into
# a throw.
sub enclosing { my $x = EVAL 'return 7'; return 8 }
is enclosing(), 7, 'an EVALd return still returns from a genuinely enclosing routine';
