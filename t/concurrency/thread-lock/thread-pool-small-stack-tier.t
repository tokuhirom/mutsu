use Test;

# ADR-0123 §6 (#17392): once the stack budget is spent, a worker that has to
# exist takes the smallest stack tier -- 4 MiB in a release build, 32 MiB in a
# debug one -- so hundreds of `start` blocks parked in `await` fit a limited
# address space (Log::Dispatch's t/040-threaded parks 500 under the ecosystem
# sandbox's `ulimit -v 6000000`). A worker on the smallest tier is still
# guarded: deep recursion there raises a catchable error instead of crashing.
# `MUTSU_STACK_BUDGET_MB=0` puts every worker on that tier.
#
# The recursion half has no rakudo counterpart: rakudo grows its call stack
# on the heap and recurses until it runs out of memory (ADR-0100).

plan 6;

my $exe = $*EXECUTABLE.absolute;

sub run-child(Str $code, *%env) {
    my %e = %*ENV;
    %e{.key} = .value for %env;
    my $p = run $exe, '-e', $code, :out, :err, :env(%e);
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out, $err)
}

{
    my $code = q:to/CODE/;
        my $max = 0;
        sub f($n) { $max = $n; f($n + 1) }
        try await start { f(0) };
        say $!.message;
        class C { method m($n) { self.m($n + 1) } }
        try await start { C.m(0) };
        say $!.message;
        sub g($n) { my @a = (1, 2).map({ g($n + 1) }) }
        try await start { g(0) };
        say $!.message;
        say $max > 100 ?? 'recursed' !! "only $max levels";
        CODE
    my ($exit, $out, $err) = run-child($code, MUTSU_STACK_BUDGET_MB => 0);
    my @lines = $out.lines;
    is @lines[0], 'Too deep recursion (out of stack space)',
        'deep sub recursion on a smallest-tier worker raises';
    is @lines[1], 'Too deep recursion (out of stack space)', '... so does method recursion';
    is @lines[2], 'Too deep recursion (out of stack space)', '... and recursion through a closure';
    is @lines[3], 'recursed', '... after a real recursion depth';
    is $exit, 0, '... and the program carries on to a clean exit';
}

# A burst of workers parked on one promise, every one of them on the
# smallest tier.
{
    my $code = q:to/CODE/;
        my $starter = Promise.new;
        my (@ready, @worker);
        for ^100 -> $id {
            @ready.push: my $ready = Promise.new;
            @worker.push: start { $ready.keep; await $starter; $id }
        }
        await @ready;
        $starter.keep;
        say [+] await @worker;
        CODE
    my ($exit, $out, $err) = run-child($code, MUTSU_STACK_BUDGET_MB => 0);
    is $out.trim, '4950', '100 parked workers on the smallest tier all finish';
}
