use v6;
use Test;

# VM-native dispatch of pure lexical &-var calls (Track A).
# These calls used to reach the tree-walking interpreter terminal; they now
# dispatch through vm_call_on_value. The cases below pin the semantics that
# originally blocked the move (dynamic vars, lexotic control flow, closure
# mutation visibility across frames).

plan 14;

# --- dynamic var rebinding visible inside a &-param call (the old blocker) ---
class FakeIO {
    has @!lines;
    method print(*@stuff) { @!lines.push(@stuff.join); True }
    method lines() { @!lines }
}

sub cap(&code) {
    my $*ERR = FakeIO.new;
    code();
    $*ERR.lines.join("|");
}

is cap({ note "hello"; note "world" }), "hello\n|world\n",
    'note inside &-param call writes to the caller-rebound $*ERR';

# --- basic lexical &-var forms ---
my &f1 = sub { return 42; 99 };
is f1(), 42, 'return inside a lexical sub';

my $x = 1;
my &f2 = { $x++ };
f2();
is $x, 2, 'captured-outer write is visible after the call';

my &fact = -> $n { $n <= 1 ?? 1 !! $n * fact($n - 1) };
is fact(5), 120, 'recursion through the lexical &-var name';

sub callit(&foo) { foo(7) }
my &double = sub dbl($n) { $n * 2 };
is callit(&double), 14, 'named sub bound to a different lexical name';

sub apply(&op, *@args) { op(|@args) }
is apply({ $^a + $^b }, 3, 4), 7, 'placeholder block via &-param with slurpy args';

# --- lexotic control flow through the VM-dispatched call ---
sub invoke(&c) { c(); "after-c" }
sub g1 {
    invoke({ return "early" });
    "late";
}
is g1(), "early", 'return inside a block &-param exits the enclosing routine';

my @seen;
sub run-once(&c) { c() }
for 1..5 -> $i {
    run-once({ last if $i == 3; @seen.push($i) });
}
is @seen.join(","), "1,2", 'last inside a &-param block breaks the enclosing loop';

sub trampoline(&c) { c() }
my @vals = gather trampoline({ take $_ for 1..3 });
is @vals.join(","), "1,2,3", 'take propagates through a &-param call into gather';

sub safely(&c) { try { c() }; $! ?? "caught" !! "ok" }
is safely({ die "boom" }), "caught", 'die inside &-param call is caught by caller try';
is safely({ 42 }), "ok", 'non-dying &-param call leaves $! clear';

# --- instance mutation inside the closure (cell visibility) ---
class Counter {
    has $.n is rw = 0;
    method bump() { $!n++ }
}
sub run3(&c) { c(); c(); c() }
my $counter = Counter.new;
run3({ $counter.bump });
is $counter.n, 3, 'instance attr mutation inside &-param call is visible in caller';

# --- nested dynamic var read through two lexical frames ---
sub outer-cap(&code) {
    my $*MARK = "outer";
    code();
}
sub inner-read() { $*MARK }
is outer-cap({ inner-read() }), "outer",
    'dynamic var declared in caller visible through nested lexical calls';

# --- per-iteration loop captures stay distinct ---
my @subs;
for 1..3 -> $i {
    @subs.push({ $i * 10 });
}
sub call-it(&c) { c() }
is @subs.map({ call-it($_) }).join(","), "10,20,30",
    'per-iteration loop captures called via &-param keep their values';
