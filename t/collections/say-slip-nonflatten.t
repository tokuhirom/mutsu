use Test;

# say/put/print/note are `**@`-slurpy: a `.Slip`/`slip(...)` VALUE passed as an
# argument stays whole (prints as a list), it does NOT flatten into separate
# arguments. Only an explicit `|EXPR` pipe flattens at the call site.
# Regression: mutsu flattened every Slip argument, so `say @a.Slip` printed
# "123" instead of "(1 2 3)".
#
# We drive real subprocesses (via $*EXECUTABLE) and compare captured stdout,
# because the flatten-vs-whole distinction is only observable in the output.

plan 15;

my $mutsu = $*EXECUTABLE.absolute;
my $n = 0;

sub out-of($code) {
    my $base = $*TMPDIR.add("slip-test-{$*PID}-{$n++}").absolute;
    spurt "$base.code", $code;
    shell "$mutsu $base.code > $base.out 2> $base.err";
    my $o = slurp "$base.out";
    unlink "$base.code"; unlink "$base.out"; unlink "$base.err";
    $o
}

# say / note gist the whole Slip as a list; put / print use its Str
is out-of('my @a=1,2,3; say @a.Slip'),   "(1 2 3)\n", 'say .Slip value -> (1 2 3)';
is out-of('my @a=1,2,3; put @a.Slip'),   "1 2 3\n",   'put .Slip value -> 1 2 3';
is out-of('my @a=1,2,3; print @a.Slip'), "1 2 3",     'print .Slip value -> 1 2 3';

# An explicit `|EXPR` pipe DOES flatten into the argument list
is out-of('my @a=1,2,3; say |@a'),   "123\n", 'say |@a flattens -> 123';
is out-of('my @a=1,2,3; put |@a'),   "123\n", 'put |@a flattens -> 123';
is out-of('my @a=1,2,3; print |@a'), "123",   'print |@a flattens -> 123';

# slip(...) / (list).Slip / a scalar holding a Slip all stay whole
is out-of('say (1,2,3).Slip'),                "(1 2 3)\n", 'say (list).Slip -> (1 2 3)';
is out-of('say slip(1,2,3)'),                 "(1 2 3)\n", 'say slip(...) -> (1 2 3)';
is out-of('my $s = (1,2,3).Slip; say $s'),    "(1 2 3)\n", 'say $s (Slip in scalar) -> (1 2 3)';

# A Slip argument does not swallow later arguments
is out-of('my @a=1,2; say @a.Slip, "X"'), "(1 2)X\n", 'Slip value + trailing arg';
is out-of('my @a=1,2,3; my @b=8,9; say |@a, @b.Slip'), "123(8 9)\n",
    'mixed pipe (flatten) + Slip value (whole)';

# empty Slip
is out-of('my @a=(); say @a.Slip'), "()\n", 'say empty .Slip -> ()';

# a typed-array hole renders through the whole Slip (doc: Type/Array.rakudoc)
is out-of('my Int @a=[0]; @a[3]=3; say @a.Slip'), "(0 (Int) (Int) 3)\n",
    'typed-array hole via say .Slip';

# a plain (non-slip) argument is unaffected
is out-of('my @a=1,2,3; say @a'), "[1 2 3]\n", 'say @a (array) unaffected';
is out-of('say 1, 2, 3'),         "123\n",     'say with three plain args -> 123';
