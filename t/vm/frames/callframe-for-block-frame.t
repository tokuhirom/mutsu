use v6;
use Test;

plan 11;

# A `for` block is its own Raku call frame: callframe(0) inside a for body is
# the block, and the enclosing routine is one level up. (Type/CallFrame.rakudoc
# "calling-frame" example.)

# --- The documented calling-frame idiom -------------------------------------
sub calling-frame() {
    for 1..* -> $level {
        given callframe($level) -> $frame {
            when $frame ~~ CallFrame {
                next unless $frame.code ~~ Routine;
                return $frame.code.package.gist;
            }
            default { return "no calling routine or method found"; }
        }
    }
}
is calling-frame(), '(GLOBAL)', 'calling-frame walk reaches the enclosing routine';

# --- Frame kinds inside a for body ------------------------------------------
sub s {
    for 1 -> $x {
        is callframe(0).code.^name, 'Block', 'callframe(0) in a for body is a Block';
        ok callframe(0).code !~~ Routine, 'the for-block frame is not a Routine';
        is callframe(1).code.^name, 'Sub', 'callframe(1) is the enclosing sub';
        ok callframe(1).code ~~ Routine, 'the enclosing frame is a Routine';
        is callframe(1).code.name, 's', 'the enclosing sub is named s';
    }
}
s();

# --- Two nested for blocks --------------------------------------------------
sub t {
    for 1 -> $a {
        for 1 -> $b {
            is callframe(0).code.^name, 'Block', 'inner for-block is a Block';
            is callframe(1).code.^name, 'Block', 'outer for-block is a Block';
            is callframe(2).code.^name, 'Sub',   't is two levels up';
            is callframe(2).code.name, 't', 'the routine two levels up is t';
        }
    }
}
t();

# A sub declared inside a for body still resets the block depth: callframe(0)
# inside it is the sub itself, not a synthetic block frame.
sub outer {
    for 1 {
        sub inner { callframe(0).code.^name }
        is inner(), 'Sub', 'a sub nested in a for body resets the frame depth';
    }
}
outer();
