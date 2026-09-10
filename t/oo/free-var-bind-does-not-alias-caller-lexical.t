use Test;

# A named sub's free variable must resolve to its LEXICAL declaration scope,
# not to whatever an intervening caller happens to have declared under the same
# name.
#
# Measured against raku v2026.07: a plain read, a plain write, and two levels of
# intervening callers all answer exactly as raku. The divergence this file was
# written for was confined to a `:=` BIND whose source is a free variable, and
# its shape was an ALIASING, not a stale read: afterwards the caller's own
# lexical and the compunit's were one container (rows N1/N2).
#
# Root cause (fixed 2026-09-07): the `:=` handlers carried the bind's shared
# cell to the source by TWO by-name routes -- a bare `env` insert that the
# call-return merge copies into the caller's tier, and the
# `propagate_bind_to_ancestor_frames` splice -- even when the source's home is
# the compunit / mainline file-scope lexical store (ADR-0024), which owns the
# name outright. Both routes deposited the cell in an intervening caller's own
# env tier, where its same-named `my` adopted it through the `GetLocal`
# lazy-sync. See `news/2026-09/free-var-bind-aliased-caller-lexical.md`.
#
# Everything is declared at file scope on purpose. The shapes below behave
# differently inside a bare block, which is a separate surface, so keeping this
# file flat is what makes it measure the thing it names.

plan 22;

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

# The rows that used to diverge. All of them were ONE mechanism: the bind
# aliased the caller's lexical to the source's container.
my $vh = 1;
my $ah;
sub g-bind() { $ah := $vh }
sub f-bind() { my $vh = 5; g-bind(); $vh }
is f-bind(), 5, 'H: the caller keeps its own lexical across a binding callee';
# ... and the bind itself must STILL alias the compunit lexical it named. This
# is the half the fix has to preserve: not writing the cell into the caller's
# tier must not cost the binding its source.
$vh = 9;
is $ah, 9, 'H2: the alias still tracks the lexical the callee actually bound';

my $vn = 1;
my $an;
sub g-alias() { $an := $vn }
sub f-alias() { my $vn = 5; g-alias(); $vn = 7; $vn }
is f-alias(), 7, 'N1: the caller can still write its own name';
is $vn, 1, 'N2: but that write must not reach the compunit lexical';

my $vo = 1;
my $ao;
sub g-lex() { $ao := $vo }
my sub f-lex() { my $vo = 5; g-lex(); $vo }
is f-lex(), 5, 'O: a lexical `my sub` caller is affected identically';

# Q: the G2 spelling (a statement after the bind, so it compiles through the
# `SetLocal` scalar-bind path rather than `SetGlobal`) must keep its alias too.
my $vq = 1;
my $aq;
sub g-q() { $aq := $vq; 0 }
sub f-q() { my $vq = 5; g-q(); $vq }
is f-q(), 5, 'Q1: same, for the bind-then-statement spelling';
$vq = 8;
is $aq, 8, 'Q2: and its alias still tracks the source';
