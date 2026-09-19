use Test;

# CLI::Ecosystem 0.0.7 uses this CLI::Version idiom: a proto declaration is
# passed as a positional argument to a module's EXPORT routine.
plan 2;

my $r = run(
    $*EXECUTABLE,
    '-I', 't/lib',
    '-e', q:to/PROGRAM/,
        use UseInlineProtoArg 'ignored', proto sub helper(|) is export {*};
        say 'loaded'
        PROGRAM
    :out,
    :err,
);

is $r.exitcode, 0, 'inline proto argument loads without a MAIN redeclaration';
is $r.out.slurp(:close).trim, "Sub\nloaded",
    'the module receives the proto object as its EXPORT argument';
