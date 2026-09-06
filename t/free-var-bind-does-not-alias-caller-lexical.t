use Test;

# A named sub's free variable must resolve to its LEXICAL declaration scope,
# not to whatever an intervening caller happens to have declared under the same
# name. `todo/deep/free-var-read-in-callee-resolves-through-dynamic-caller-chain.md`
# describes this as a general failure of free-variable resolution ("mutsu's Env
# is a dynamic chain, so a free read walks g -> f -> mainline").
#
# Measured against raku v2026.07 on 2026-09-06, that is NOT what happens: a
# plain read, a plain write, and two levels of intervening callers all answer
# exactly as raku. The divergence is confined to a `:=` BIND whose source is a
# free variable and whose target is also free -- and its shape is an ALIASING,
# not a stale read: afterwards the caller's own lexical and the compunit's are
# one container (rows N1/N2).
#
# Everything is declared at file scope on purpose. The shapes below behave
# differently inside a bare block, which is a separate surface, so keeping this
# file flat is what makes it measure the thing it names.
#
# The rows that already agree are pinned so the correct half cannot regress
# while the remaining three are worked; the three that do not are `todo`.

plan 19;

# A: a callee reads a free variable while a caller shadows the name.
my $va = 1;
my $seen;
sub g-read() { $seen = $va }
sub f-read() { my $va = 5; g-read(); $va }
is f-read(), 5, 'A1: the caller keeps its own lexical across the call';
is $seen, 1, 'A2: the callee read its own lexical scope, not the caller';

# B: a callee WRITES the free variable.
my $vb = 1;
sub g-write() { $vb = 99 }
sub f-write() { my $vb = 5; g-write(); $vb }
is f-write(), 5, 'B1: a callee write does not reach a shadowing caller lexical';
is $vb, 99, 'B2: it reaches the compunit lexical it names';

# F: two levels of intervening callers, each with its own shadow.
my $vf = 1;
my @seq;
sub g-deep() { @seq.push($vf) }
sub h-deep() { my $vf = 7; g-deep(); @seq.push($vf) }
sub f-deep() { my $vf = 5; h-deep(); @seq.push($vf) }
f-deep();
is @seq, [1, 7, 5], 'F: each frame sees its own binding at any nesting depth';

# G: a bind whose TARGET is a local of the callee. This pair is the sharpest
# discriminator in the file, and it is not about the target at all: the two
# callees differ ONLY in whether the bind is the last statement of the routine.
my $vg = 1;
sub g-bind-last() { my $t := $vg }
sub f-bind-last() { my $vg = 5; g-bind-last(); $vg }
is f-bind-last(), 5, 'G1: a bind as the callee\'s last statement leaves the caller alone';

my $vg2 = 1;
sub g-bind-then-stmt() { my $t := $vg2; 0 }
sub f-bind-then-stmt() { my $vg2 = 5; g-bind-then-stmt(); $vg2 }
todo 'ANY statement after the bind makes it reach the caller';
is f-bind-then-stmt(), 5, 'G2: a statement after the bind must not change that';

# I: the same bind performed in the mainline instead of in a callee.
my $vi = 1;
my $ai;
$ai := $vi;
sub f-mainline-bind() { my $vi = 5; $vi }
is f-mainline-bind(), 5, 'I1: a mainline bind does not reach into a later call';
$vi = 9;
is $ai, 9, 'I2: and the mainline bind still aliases';

# J: the bind in the other direction (the free variable is the TARGET).
my $vj = 1;
my $bj = 3;
sub g-reverse() { $vj := $bj }
sub f-reverse() { my $vj = 5; g-reverse(); $vj }
is f-reverse(), 5, 'J: binding INTO a free variable leaves the caller alone';

# P: the caller declares its lexical AFTER the call.
my $vp = 1;
my $ap;
sub g-late() { $ap := $vp }
sub f-late() { g-late(); my $vp = 5; $vp }
is f-late(), 5, 'P: a later declaration is unaffected';

# S / U: a caller's own writes stay its own, with and without a callee write.
my $vs = 1;
sub f-own-write() { my $vs = 5; $vs = 7; $vs }
is f-own-write(), 7, 'S1: a routine writes its own lexical';
is $vs, 1, 'S2: and the compunit lexical is untouched';

my $vu = 1;
sub g-both() { $vu = 99 }
sub f-both() { my $vu = 5; g-both(); $vu = 7; $vu }
is f-both(), 7, 'U1: the two writes stay independent';
is $vu, 99, 'U2: each landing in its own scope';

# The three that still diverge. All three are ONE mechanism: the bind aliases
# the caller's lexical to the source's container.
my $vh = 1;
my $ah;
sub g-bind() { $ah := $vh }
sub f-bind() { my $vh = 5; g-bind(); $vh }
todo 'a free-variable := bind aliases an intervening caller lexical';
is f-bind(), 5, 'H: the caller keeps its own lexical across a binding callee';

my $vn = 1;
my $an;
sub g-alias() { $an := $vn }
sub f-alias() { my $vn = 5; g-alias(); $vn = 7; $vn }
is f-alias(), 7, 'N1: the caller can still write its own name';
todo 'the write reaches the compunit lexical too -- they became one container';
is $vn, 1, 'N2: but that write must not reach the compunit lexical';

my $vo = 1;
my $ao;
sub g-lex() { $ao := $vo }
my sub f-lex() { my $vo = 5; g-lex(); $vo }
todo 'same mechanism; not specific to a mainline-scoped caller';
is f-lex(), 5, 'O: a lexical `my sub` caller is affected identically';
