use Test;

# ADR-0004 J4d's light-call frame-reuse latch proves "this frame wrote no env by
# name" from the shape of its scoped overlay. A full method dispatch runs
# `flatten_scoped_env` first, which produces a `parent: None` env and so used to
# disarm that latch for the rest of the frame's life: the return merge then ran
# over the WHOLE flattened scope instead of over the callee's own writes (#7630).
# The latch now travels with the env across the flatten, which means the unwind
# takes a different arm than before for any routine that dispatches a method.
#
# These pin the merge rules that arm must keep obeying -- what a callee may leak
# into its caller and what it must still hand back -- with a full method dispatch
# (a user-defined method on an instance, which no fast path can answer) somewhere
# in every body.

plan 19;

class Counter {
    has $.n is rw;
    method bump() { $!n = $!n + 1 }
}
my $c = Counter.new(n => 0);

# --- a callee-local must not leak, however the frame unwinds ----------------
my $a = 'caller';
sub shadowing() { my $a = 'callee'; $c.bump(); $a }
is shadowing(), 'callee', 'a callee-local shadowing a caller lexical reads its own value';
is $a, 'caller', 'and does not overwrite the caller lexical after a method dispatch';

sub declaring-fresh() { my $only-here = 'x'; $c.bump(); $only-here }
is declaring-fresh(), 'x', 'a callee-only local is visible inside the callee';
nok (try MY::<$only-here>).defined, 'and does not appear in the caller scope';

# --- a captured-outer by-name write must still reach the caller -------------
my $seen = 0;
sub writes-outer($v) { $c.bump(); $seen = $v }
writes-outer(41);
is $seen, 41, 'a callee write to a captured outer lexical survives the return merge';

my @log;
sub pushes-outer($v) { $c.bump(); @log.push($v) }
pushes-outer('a');
pushes-outer('b');
is @log.join(','), 'a,b', 'a callee write to a captured outer array survives too';

# ... including when the write happens BEFORE the dispatch, so it sits in the
# overlay the flatten collapses.
my $before = 0;
sub writes-then-dispatches($v) { $before = $v; $c.bump(); $before }
is writes-then-dispatches(7), 7, 'a write made before the dispatch is readable after it';
is $before, 7, 'and still reaches the caller';

# --- the per-frame private names stay private -------------------------------
sub topic-untouched() { $_ = 'callee-topic'; $c.bump(); $_ }
$_ = 'caller-topic';
is topic-untouched(), 'callee-topic', 'the callee gets its own topic';
is $_, 'caller-topic', 'and the caller keeps its own across the merge';

sub bang-private() { $c.bump(); try die 'inner'; $!.Str }
$! = Nil;
like bang-private(), /inner/, 'the callee sees its own $!';
nok $!.defined, 'and the caller keeps its own $! after the merge';

# --- the whole lexical view must survive the flatten ------------------------
sub capture-after-dispatch() {
    my $outer = 41;
    $c.bump();
    my $add = sub { $outer + 1 };
    $add();
}
is capture-after-dispatch(), 42, 'a closure created after a dispatch still sees the frame lexicals';

sub stash-after-dispatch() {
    my $here = 7;
    $c.bump();
    MY::<$here>;
}
is stash-after-dispatch(), 7, 'MY:: after a dispatch still resolves the frame lexical';

# --- nesting and recursion --------------------------------------------------
sub inner-call($n) { $c.bump(); $n * 2 }
sub outer-call($n) { my $t = inner-call($n); $c.bump(); $t + 1 }
is outer-call(5), 11, 'a light call nested inside a dispatching frame returns its own value';

sub countdown($n) { $c.bump(); $n <= 0 ?? 0 !! $n + countdown($n - 1) }
is countdown(4), 10, 'recursion through a dispatching frame is unaffected';

# --- the dispatch really is a dispatch, not something a fast path answered ---
my $n0 = $c.n;
shadowing();
is $c.n, $n0 + 1, 'the dispatch inside a light-called frame really runs the method body';

# --- the workload the ticket measured still computes ------------------------
class C2 { has $.n is rw; method bump() { $!n = $!n + 1 } }
sub work($o) { my $x = 1; my $y = 2; $o.bump(); $x + $y }
my $c2 = C2.new(n => 0);
my $sum = 0;
for ^20 { $sum += work($c2) }
is $sum, 60, 'the measured workload returns the same sum';
is $c2.n, 20, 'and mutated the receiver once per iteration';
