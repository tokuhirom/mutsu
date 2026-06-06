use v6;
use Test;

# Regression pin for Slice 6.1: removing the blanket per-call env_dirty=true /
# bind-time mark-all-params-dirty churn from the compiled call fast paths
# (positional_light / light / OTF). A pure compiled call must do zero dual-store
# sync, but a compiled call that mutates a *captured outer* variable must still
# propagate that write (the scoped-overlay merge sets env_dirty, so the caller
# re-syncs its locals). See docs/vm-dual-store.md (Slice 6).

plan 12;

# --- pure recursive function returns correct value (no sync needed) ---
sub fib($n) { $n < 2 ?? $n !! fib($n-1) + fib($n-2) }
is fib(10), 55, 'pure recursive positional call';
is fib(15), 610, 'pure recursive positional call (deeper)';

# --- compiled call mutating a captured outer scalar must propagate ---
my $counter = 0;
sub bump() { $counter++ }
bump(); bump(); bump();
is $counter, 3, 'captured-outer scalar mutation propagates (zero-arg)';

my $acc = 0;
sub add-to($n) { $acc += $n }
add-to(5); add-to(7);
is $acc, 12, 'captured-outer scalar mutation propagates (positional)';

# --- the caller observes the mutated value immediately after the call,
#     interleaved with reads of its own locals (re-sync correctness) ---
my $total = 0;
sub addup($x) { $total += $x }
my $local-sum = 0;
for 1..5 -> $i {
    addup($i);
    $local-sum += $i;
}
is $total, 15, 'interleaved captured-outer mutation across loop';
is $local-sum, 15, 'caller local untouched by callee env churn';

# --- captured array/hash mutation ---
my @log;
sub record($x) { @log.push($x) }
record('a'); record('b'); record('c');
is @log.join(','), 'a,b,c', 'captured-outer array mutation propagates';

my %seen;
sub mark($k) { %seen{$k} = True }
mark('x'); mark('y');
is %seen.keys.sort.join(','), 'x,y', 'captured-outer hash mutation propagates';

# --- named-param compiled call (light path) mutating outer ---
my $named-acc = 0;
sub nadd(:$v) { $named-acc += $v }
nadd(v => 3); nadd(v => 4);
is $named-acc, 7, 'captured-outer mutation via named-param (light) call';

# --- nested calls: inner mutates, outer reads ---
my $deep = 0;
sub inc() { $deep++ }
sub twice() { inc(); inc() }
twice(); twice();
is $deep, 4, 'nested compiled calls propagate captured mutation';

# --- a pure call followed by reading the same outer var that a *prior* call
#     mutated (ensures no stale-locals after the no-sync pure call) ---
my $state = 100;
sub mutate() { $state = 200 }
sub pure($n) { $n * 2 }
mutate();
my $p = pure(5);
is $state, 200, 'outer mutation visible after an interleaved pure call';
is $p, 10, 'pure call result correct';
