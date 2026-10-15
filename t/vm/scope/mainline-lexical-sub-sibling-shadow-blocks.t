use Test;

# A named sub reads the mainline lexical visible where it is declared, however
# many blocks shadow the name before it is called (ADR-0024, ADR-12529 phase
# 3). The first shadowing `if` / loop body put the captured cell into every
# same-named slot on exit, and the next sibling block's `my $q` was then taken
# for a re-declaration of the captured variable: the capture was dropped and
# the sub read the caller's `$q` from then on.
plan 11;

my $q = 'top';
sub f { $q }
sub g($x) { $q }
sub set-q($v) { $q = $v }

if True { my $q = 'if1'; is f(), 'top', 'first shadowing if block' }
if True { my $q = 'if2'; is f(), 'top', 'second shadowing if block' }
for 1 { my $q = 'loop'; is f(), 'top', 'a later loop body' }
for 1, 2 { my $q = "loop$_"; is g($_), 'top', "loop iteration $_, sub with a parameter" }
{ my $q = 'blk'; is f(), 'top', 'a later bare block'; }
if True { my $q = 'if3'; set-q('written'); is $q, 'if3', 'the sub writes the mainline $q, not the shadow' }
is $q, 'written', 'the write reached the mainline lexical';
is f(), 'written', 'and the sub reads it back';
$q = 'live';
if True { my $q = 'if4'; is f(), 'live', 'a mainline write after the blocks is still seen' }
{ my $q = 'tail'; is f(), 'live', 'a tail-position block' }
