use Test;

# A module's `sub EXPORT` runs once per `use` statement, when the statement is
# compiled (ADR-17558), as on rakudo: a `use` inside a loop body or a routine
# installs the map that compile-time run returned every time the block runs,
# instead of running the hook again on each entry. A second `use` of a module
# that is already loaded runs the hook at its own compile, too.

plan 2;

my $program = q:to/END/;
    say 'start';
    for ^3 { use HookRunsOnce; say hook-value() }
    sub f { use HookRunsOnce; hook-value() }
    say f() for ^2;
    END
my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $program, :out, :err;
is $proc.out.slurp(:close),
    "EXPORT ran\nEXPORT ran\nstart\n42\n42\n42\n42\n42\n",
    'each `use` runs the hook once, at compile time, before the mainline';
is $proc.exitcode, 0, 'the program exits cleanly';
