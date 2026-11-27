use Test;
use lib 't/lib';
use EvalBacktraceFixture;

# An EVAL'd string is a compilation unit of its own, and its mainline is a
# `<unit>` frame of its own in `.backtrace`, named `EVAL_<n>` and at the line
# within the snippet -- also when the EVAL runs inside a module routine, and
# also when the exception is thrown natively rather than by `die` (#19242).
# A REPL (Rakuish's `t/82-errors-recall.t`) locates an error inside a cell by
# that frame. A snippet that fails to compile still has a `.backtrace`.

plan 13;

sub eval-frames($ex) {
    $ex.backtrace.list.grep({ .file.starts-with('EVAL') && .subname eq '<unit>' })
}

{
    my $ex;
    try { EVAL "my \$x = 1;\nsay 1/0"; CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::Numeric::DivideByZero, 'a native throw inside EVAL';
    my @f = eval-frames($ex);
    is @f.elems, 1, 'has one EVAL <unit> frame';
    is @f[0].line, 2, 'at the line within the snippet';
    my @caller = $ex.backtrace.list.grep({ .file.ends-with('eval-unit-backtrace-frame.t') });
    is @caller[0].line, 20, 'the caller frame is at the EVAL call';
}

{
    my $ex = EvalBacktraceFixture.run("my \$y = 1;\nsay 1/0");
    my @f = eval-frames($ex);
    is @f.elems, 1, 'an EVAL inside a module method has its own frame';
    is @f[0].line, 2, 'at the snippet line, not attributed to the module';
    my @mod = $ex.backtrace.list.grep({ .file.contains('EvalBacktraceFixture') });
    ok @mod.elems >= 1, 'the module routine frame is still there';
}

{
    my $ex;
    try { EVAL "sub g \{ die 'g' }\n\ng()"; CATCH { default { $ex = $_ } } }
    my @named = $ex.backtrace.list.grep({ .subname eq 'g' });
    is @named[0].line, 1, 'a sub declared in the EVAL is at its own line';
    is eval-frames($ex)[0].line, 3, 'the EVAL unit frame is at the call of it';
}

{
    my $ex = EvalBacktraceFixture.run('die "boom"');
    is eval-frames($ex)[0].line, 1, 'die inside a module-run EVAL';
}

{
    my $ex;
    try { EVAL 'say 1 2'; CATCH { default { $ex = $_ } } }
    ok $ex ~~ X::Comp, 'a compile failure inside EVAL';
    isa-ok $ex.backtrace, Backtrace, 'still has a Backtrace';
    nok eval-frames($ex), 'with no frame for the unit that never ran';
}
