use Test;

# Track B/Track C: `state @a` / `state %h` live in a shared ContainerRef cell
# once a thread context is active (StateVarInit). These pin that aggregate
# STATE accumulates through the cell: block-exit writeback goes INTO the cell
# (set_state_var write-through) and `@a.push` under shared context appends
# through the cell. Regressions fixed here (deterministic pre-fix):
#   - %h<k>++ / @a.push returned 1,1,1... once any thread had spawned
#   - push died ("No such method 'push'") when the cell was seeded non-empty
#     via the thread-spawn state migration.
# Deliberately NOT tested: heavily contended concurrent structural inserts —
# outside language guarantees (real rakudo crashes with a MoarVM oops on that
# shape; mutsu loses some updates but never corrupts).

plan 18;

# --- Track B slice 3: state aggregates live in a ContainerRef cell in ALL
# modes (not just under a thread context), so sibling closures returned from
# the same factory share ONE live container, matching raku. Pre-fix these
# accumulated per-closure (mk() twice gave 1,1,2 instead of 1,2,3). ---

sub mk-arr() { state @a; -> { @a.push(1); @a.elems } }
my $a1 = mk-arr();
my $a2 = mk-arr();
is $a1(), 1, 'closure-shared state @: first closure first call';
is $a2(), 2, 'closure-shared state @: sibling closure sees prior push';
is $a1(), 3, 'closure-shared state @: first closure keeps accumulating';

sub mk-hash() { state %h; -> { %h<k>++; %h<k> } }
my $h1 = mk-hash();
my $h2 = mk-hash();
is $h1(), 1, 'closure-shared state %: first closure first call';
is $h2(), 2, 'closure-shared state %: sibling closure sees prior increment';
is $h1(), 3, 'closure-shared state %: first closure keeps accumulating';

# Whole-aggregate assignment as the FIRST op after init must write through
# the cell (regression: the env insert was skipped when the fresh cell
# compared PartialEq-equal to the pre-seeded plain empty aggregate, so the
# assignment landed in a detached snapshot and reads gave 0).
sub wa() { state @a; @a = 9,8; @a.elems }
is wa(), 2, 'whole-array assignment on a fresh state cell is visible';

sub wh() { state %h; %h = (k => 1); %h.elems }
is wh(), 1, 'whole-hash assignment on a fresh state cell is visible';

# Pre-thread baseline: plain state store, and a seed for the migration case.
sub pa() { state @p; @p.push('x'); @p.elems }
is pa(), 1, 'pre-thread state push (plain store)';

# Force the shared (threaded) context on: state vars now live in cells.
my @warm = (1..1).map: { Thread.start({ 1 }) };
.finish for @warm;

# Migration: the pre-thread value seeded the cell; pushing onto the
# non-empty celled array must keep accumulating (died pre-fix).
is pa(), 2, 'state seeded before thread spawn migrates into the cell';
is pa(), 3, 'celled state array keeps accumulating after migration';

sub gh() { state %h; %h<k>++; %h<k> }
is gh(), 1, 'state %h under shared ctx: first call';
is gh(), 2, 'state %h accumulates through the cell';
is gh(), 3, 'state %h keeps accumulating';

sub ga() { state @a; @a.push(1); @a.elems }
is ga(), 1, 'state @a under shared ctx: first push';
is ga(), 2, 'state @a push goes through the cell';

sub gs() { state $n = 0; $n++; $n }
gs();
is gs(), 2, 'scalar state still accumulates (unchanged path)';

# Sequential cross-thread sharing: each thread's element write lands in the
# same cell the parent reads afterwards (threads serialized => deterministic).
sub tf($x) { state %seen; %seen{$x} = 1; %seen.keys.sort.join(",") }
for 1..3 { Thread.start({ tf($_) }).finish }
is tf(0), '0,1,2,3', 'sequential threads share one state hash cell';
