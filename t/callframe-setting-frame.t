use v6;
use Test;

plan 6;

# The synthetic "setting" frame sits one level above the unit mainline. It
# reports line 1 (where control entered the unit) and a `Mu` code object.
# (Type/CallFrame.rakudoc method-line example.)

# At the top level, callframe(1) is the setting frame.
is callframe(1).line, 1, 'callframe(1).line at top level is 1 (setting frame)';
is callframe(1).annotations<line>, 1, 'the setting frame annotation matches .line';
is callframe(1).code.^name, 'Mu', 'the setting frame code is Mu';
ok callframe(1) ~~ CallFrame, 'the setting frame is a CallFrame';

# One deeper is out of call information: callframe(2) at the top level is Mu.
ok callframe(2) ~~ Mu, 'callframe(2) past the setting frame is Mu';

# From inside a sub, the setting frame is one past the mainline caller.
sub s { callframe(2) }
ok s() ~~ CallFrame, 'callframe(2) from a top-level sub is the setting frame';
