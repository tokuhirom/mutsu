use v6;
use Test;

# A module that imports through a `sub EXPORT` hook loads when the parser
# reaches its `use`, and the hook runs then, once per `use` (ADR-17558): the
# `use`'s import installs the map that run returned instead of running the
# hook again. Each case runs a child process so the order of the module's
# output is visible; every expectation is what rakudo prints.

plan 5;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    (exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    my %r = run-code(q:to/CODE/);
        say "mainline";
        use HookRunsOncePerUse;
        say hook-runs-once();
        CODE
    is %r<out>, "HookRunsOncePerUse loaded\nEXPORT ran with ()\nmainline\nimported\n",
        'the hook runs once, at compile time, and its map is imported';
}

{
    my %r = run-code(q:to/CODE/);
        use HookRunsOncePerUse;
        { use HookRunsOncePerUse; say hook-runs-once() }
        say hook-runs-once();
        CODE
    is %r<out>, "HookRunsOncePerUse loaded\nEXPORT ran with ()\nEXPORT ran with ()\nimported\nimported\n",
        'each use runs the hook once, a block\'s too';
}

{
    my %r = run-code(q:to/CODE/);
        use HookRunsOncePerUse;
        my $x = ;
        CODE
    is %r<out>, "HookRunsOncePerUse loaded\nEXPORT ran with ()\n",
        'the hook has run when a later syntax error is found';
    isnt %r<exitcode>, 0, 'the syntax error still fails the program';
}

{
    my %r = run-code(q:to/CODE/);
        say "mainline";
        use HookRunsOncePerUse <a b>;
        say hook-runs-once();
        CODE
    is %r<out>, "HookRunsOncePerUse loaded\nEXPORT ran with (a,b)\nmainline\nimported\n",
        'a literal argument list reaches the compile-time run of the hook';
}
