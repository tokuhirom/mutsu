use v6;
use Test;

# Regression pin for the scoped/overlay env on the 0-arg compiled-helper fast
# path (call_compiled_function_fast). See docs/vm-dual-store.md (Slice 6).
# A 0-arg sub with locals and no inner subs / reflective access runs under a
# scoped overlay env: its writes must not leak to the caller, while captured
# outer-variable mutations must persist.

plan 18;

# --- callee-local writes do not leak; repeated calls are independent ---
sub helper() {
    my $x = 10;
    my $y = 20;
    $x + $y;
}
is helper(), 30, 'fast-path helper returns local sum';
is helper(), 30, 'second call independent (no leak)';

# --- a local shadowing a caller var must not clobber the caller ---
my $v = 100;
sub shadow() { my $v = 1; $v }
is shadow(), 1, 'shadowing local seen inside callee';
is $v, 100, 'caller var unchanged by shadowing local';

# --- captured outer mutation persists through the scoped merge ---
my $counter = 0;
sub bump() { my $tmp = 5; $counter = $counter + $tmp; }
bump();
bump();
is $counter, 10, 'captured outer var mutation persists across calls';

# --- compound assignment on captured outer ---
my $acc = 0;
sub addone() { my $u = 1; $acc += $u; }
sub addtwice() { my $w = 0; addone(); addone(); }
addtwice();
is $acc, 2, 'nested helpers both mutate the same captured outer';

# --- routine $_ scoping: callee gets its own topic ---
$_ = 'caller-topic';
sub topic-helper() { my $z = 99; $z }
is topic-helper(), 99, 'topic-helper returns local';
is $_, 'caller-topic', "caller's \$_ restored after routine call";

# --- nested fast calls compose ---
sub inner() { my $a = 3; $a * 2 }
sub outer() { my $b = inner(); $b + 1 }
is outer(), 7, 'nested 0-arg fast calls compose';

# --- state variable persistence in a 0-arg fast helper ---
sub seq() { state $n = 0; $n++; $n }
is seq(), 1, 'state var first call';
is seq(), 2, 'state var second call';
is seq(), 3, 'state var third call';

# --- method call from inside a scoped fast helper (env flatten guard) ---
sub mhelper() { my $s = 'abc'; $s.uc }
is mhelper(), 'ABC', 'method call inside scoped helper works';

# --- EVAL inside helper falls back to non-scoped path (reflective) ---
my $oe = 42;
sub with-eval() { my $l = 7; EVAL('$oe + $l') }
is with-eval(), 49, 'EVAL inside helper reads outer+local correctly';

# --- helper returning a closure (has_inner_subs => non-scoped path) ---
sub make-adder() { my $base = 100; -> $x { $base + $x } }
my &add = make-adder();
is add(5), 105, 'closure-returning helper captures base';
is add(10), 110, 'returned closure reusable';

# --- captured array mutation through a fast helper ---
my @log;
sub a() { my $t = 1; @log.push('a'); }
sub b() { my $t = 2; a(); @log.push('b'); }
b();
is @log.join(','), 'a,b', 'captured array mutated through nested fast helpers';

# --- deep recursion is unaffected (positional-light path, not scoped-fast) ---
sub fib($n) { $n < 2 ?? $n !! fib($n - 1) + fib($n - 2) }
is fib(15), 610, 'recursion still correct';
