use Test;

# ADR-17392 (#17392): under an address-space budget every user-code thread
# starts on a small stack and continues a deep call on fresh stack segments,
# up to the depth of the 256 MiB main stack. So a `start` block recurses as
# deep as the mainline does, whichever worker runs it. A runaway recursion
# still ends in the catchable "Too deep recursion" (ADR-0100), and so does a
# segment the budget cannot afford.
#
# `MUTSU_STACK_BUDGET_MB` sets the budget a `ulimit -v` would derive.
#
# The recursion limits have no rakudo counterpart: rakudo grows its call
# stack on the heap and recurses until it runs out of memory (ADR-0100).

plan 8;

my $exe = $*EXECUTABLE.absolute;

sub run-child(Str $code, *%env) {
    my %e = %*ENV;
    %e{.key} = .value for %env;
    my $p = run $exe, '-e', $code, :out, :err, :env(%e);
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out, $err)
}

# Eight workers parked in `await` first, so the recursing one is created
# after them. With fixed stack tiers (ADR-0123 D2) it got whatever small tier
# was left and stopped after a few hundred levels.
my $depth-code = q:to/CODE/;
    my $gate = Promise.new;
    my @ready;
    my @parked = (^8).map: {
        my $ready = Promise.new;
        @ready.push: $ready;
        start { $ready.keep; await $gate }
    };
    await @ready;
    my $max = 0;
    sub f($n) { $max = $n; f($n + 1) }
    try f(0);
    my $main = $max;
    $max = 0;
    try await start { f(0) };
    say $!.message;
    say $max >= $main ?? 'as deep' !! "start $max vs main $main";
    sub g($n) { $n == 0 ?? 0 !! 1 + g($n - 1) }
    say await start { g($main div 2) };
    say $main div 2;
    $gate.keep;
    await @parked;
    CODE

{
    my ($exit, $out, $err) = run-child($depth-code, MUTSU_STACK_BUDGET_MB => 600);
    my @lines = $out.lines;
    is @lines[0], 'Too deep recursion (out of stack space)',
        'runaway recursion in a start block raises under a budget';
    is @lines[1], 'as deep',
        '... after recursing as deep as the mainline, on a worker created after eight parked ones';
    is @lines[2], @lines[3], 'a deep recursion that fits returns its value';
    is $exit, 0, '... and the program exits cleanly'
        or diag $err;
}

# A budget too small for any segment: the recursion stops on the initial
# stack, with the same catchable error.
{
    my $code = q:to/CODE/;
        sub f($n) { f($n + 1) }
        try await start { f(0) };
        say $!.message;
        CODE
    my ($exit, $out, $err) = run-child($code, MUTSU_STACK_BUDGET_MB => 0);
    is $out.lines[0], 'Too deep recursion (out of stack space)',
        'with no budget for a segment, deep recursion still raises';
    is $exit, 0, '... and the program exits cleanly'
        or diag $err;
}

# A renderer recursing through a deep structure inside one call continues on
# segments too.
{
    my $code = q:to/CODE/;
        my $a = 1;
        $a = 1 => $a for ^20000;
        say $a.raku.chars == (await start { $a.raku }).chars ?? 'same' !! 'differs';
        CODE
    my ($exit, $out, $err) = run-child($code, MUTSU_STACK_BUDGET_MB => 2048);
    is $out.trim, 'same', 'a deep .raku in a start block renders in full';
    is $exit, 0, '... and the program exits cleanly'
        or diag $err;
}
