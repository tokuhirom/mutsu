use v6;
use Test;

plan 9;

# A `{ … }` closure inside an interpolating string is its own Block call frame
# in Raku: `callframe(0)` inside it is the block, the enclosing routine is one
# level up. Test::Output relies on this with `"… {callframe(4).line}"`.

sub inner() { "{callframe(0).code.^name} {callframe(1).code.^name}" }
is inner(), 'Block Sub', 'callframe(0) in an interpolation block is the Block';

sub named() { "{callframe(1).code.name}" }
is named(), 'named', 'callframe(1) in an interpolation block is the routine';

# Walking past the routine chain still reaches the mainline and then the
# synthetic setting frame (line 1) instead of running out early.
sub deep() { "line {callframe(4).line}" }
sub mid() { deep() }
is mid(), 'line 1', 'callframe(4) through an interpolation block reaches the setting frame';

sub deep-mainline() { "{callframe(3).line}" }
sub mid-mainline() { deep-mainline() }
is mid-mainline(), '24', 'callframe(3) is the unit mainline call site';

# Nested interpolation blocks are two frames.
sub nested() { "{ "{callframe(1).code.^name} {callframe(2).code.^name}" }" }
is nested(), 'Block Sub', 'nested interpolation blocks each add a frame';

# A `for` body plus an interpolation block stack.
sub in-for() { my $r; for ^1 { $r = "{callframe(2).code.^name}" }; $r }
is in-for(), 'Sub', 'interpolation block inside a for body is two frames deep';

# Variable interpolation is not a block.
sub plain() { my $x = 'v'; "$x {callframe(0).code.^name}" }
is plain(), 'v Block', 'variable parts do not add frames';

# Block scoping of the closure body is preserved.
sub scoped() { my $x = 5; "{my $y = 3; $y + $x}" }
is scoped(), '8', 'multi-statement closure body sees outer lexicals';
sub count-it() { "{$++}" }
count-it();
is count-it(), '0', 'anon state in an interpolation block restarts per call';
