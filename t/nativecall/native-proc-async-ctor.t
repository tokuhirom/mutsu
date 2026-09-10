use Test;

# Pin for the VM-native `Proc::Async.new` fast path (ledger §D ③ ctor). The VM
# gate (`try_native_builtin_construct`) and the interpreter's `dispatch_new` arm
# both call the single static `build_native_proc_async_value` helper, so the
# native and slow paths must stay byte-identical. Construction is pure data —
# the process is only spawned later, by `.start`.

plan 8;

# Basic construction yields a Proc::Async.
my $p = Proc::Async.new('echo', 'hello', 'world');
isa-ok $p, Proc::Async, 'Proc::Async.new builds a Proc::Async';

# stdout/stderr are tappable Supplies before start.
my @out;
$p.stdout.tap(-> $line { @out.push($line) });
ok @out == 0, 'no output before start';

# Running the process feeds the tapped stdout.
my $proc = await $p.start;
is @out.join.chomp, 'hello world', 'echo output is captured';
is $proc.exitcode, 0, 'successful exit code';

# A second, independent Proc::Async is not aliased to the first.
my $p2 = Proc::Async.new('echo', 'second');
my @out2;
$p2.stdout.tap(-> $l { @out2.push($l) });
await $p2.start;
is @out2.join.chomp, 'second', 'independent Proc::Async runs separately';
is @out.join.chomp, 'hello world', 'first Proc::Async output is unaffected';

# `:w` construction allows writing to the child stdin.
my $cat = Proc::Async.new(:w, 'cat');
my @cout;
$cat.stdout.tap(-> $l { @cout.push($l) });
my $pr = $cat.start;
await $cat.write('piped-input'.encode);
$cat.close-stdin;
await $pr;
is @cout.join.chomp, 'piped-input', ':w Proc::Async round-trips stdin to stdout';

# A non-zero exit is reported.
my $false = Proc::Async.new('false');
$false.stdout.tap(-> $l { });
my $fp = await $false.start;
isnt $fp.exitcode, 0, 'non-zero exit code is reported';
