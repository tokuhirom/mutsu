use Test;

# A closure passed as an argument to a custom user-defined word-form infix
# operator (`sub infix:<times>(Int $num, &closure) {...}`, called as
# `20 times { $value++ }`) must write its mutation of an outer lexical back
# to the caller's scope once the statement finishes — exactly like passing
# the same closure to an ordinary named sub call already does. The mutation
# was visible while still inside the callee's dynamic extent, but reading the
# outer variable AFTER the statement returned saw the stale, never-refreshed
# value: `exec_infix_func_op` (the `OpCode::InfixFunc` handler) never drained
# `pending_rw_writeback_sources` into the caller's local slot the way every
# other call-opcode handler (`OpCode::CallFunc`, `OpCode::ExecCall`) does.
# Found via the `PSpec` distribution's own test suite (its `times`/`xxx`
# helpers are exactly this shape).

plan 4;

{
    sub infix:<my-times>(Int $num, &closure) {
        for ^$num { closure() }
    }
    my $value = 0;
    20 my-times { $value++ }
    is $value, 20, 'closure arg to a custom infix writes back to the outer lexical';
}

# The lexical-`&infix:<op>`-shadow early-return path.
{
    my $value = 0;
    sub apply(&op, $num) { op($num) }
    my &infix:<my-shadow> = -> $num, &closure { closure(); $num };
    my $value2 = 0;
    5 my-shadow { $value2++ };
    is $value2, 1, 'closure arg through a lexical &infix:<op> override writes back too';
}

# Multiple sequential custom-infix calls with closures don't cross-clobber.
{
    sub infix:<my-times2>(Int $num, &closure) { for ^$num { closure() } }
    my $a = 0;
    my $b = 0;
    3 my-times2 { $a++ }
    5 my-times2 { $b++ }
    is $a, 3, 'first custom-infix closure call writes back correctly';
    is $b, 5, 'second custom-infix closure call writes back correctly, independent of the first';
}
