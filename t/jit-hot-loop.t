use v6;
use Test;

# JIT J4b hot-loop entry (ADR-0004, vm_jit::try_enter_range): compound-loop
# bodies at the top level run through run_range once per iteration and get
# JIT-compiled when hot. Every loop here runs >= 300 iterations so a
# MUTSU_JIT=on run (any threshold <= 300) executes the native body; with the
# JIT off the same assertions pin the interpreter. Cases cover the fast
# paths plus the bail/resume seams: `last`/`next` (body bails out of the
# JIT), branches, nested loops, and accumulation across iterations.

plan 10;

my $sum = 0;
for ^1000 {
    $sum += $_ * 3 + 1;
}
is $sum, 1_499_500, 'for ^N accumulation (int-arith shape)';

my $i = 0;
my $acc = 0;
while $i < 500 {
    $acc = $acc + $i;
    $i = $i + 1;
}
is $acc, 124_750, 'while loop with JIT-eligible cond and body';

my $evens = 0;
my $odds = 0;
for ^600 {
    if $_ %% 2 { $evens++ } else { $odds++ }
}
is $evens, 300, 'branchy body: even count';
is $odds, 300, 'branchy body: odd count';

my $stopped = 0;
for ^1000 {
    last if $_ == 400;
    $stopped = $_;
}
is $stopped, 399, 'last aborts a hot loop (body stays interpreted)';

my $kept = 0;
for ^500 {
    next if $_ %% 2;
    $kept++;
}
is $kept, 250, 'next skips iterations correctly';

my $nested = 0;
for ^100 {
    for ^100 {
        $nested++;
    }
}
is $nested, 10_000, 'nested loops: inner body JIT-compiles independently';

my $s = '';
for ^300 {
    $s ~= 'x';
}
is $s.chars, 300, 'string building in a hot body';

my $n = 0e0;
for ^400 {
    $n += 0.5e0;
}
is $n, 200e0, 'Num accumulation in a hot body';

loop (my $j = 0; $j < 350; $j++) {
    $sum++;
}
is $sum, 1_499_850, 'C-style loop body/cond/step through the range JIT';
