use Test;

# A slang action reports a compile-time error with `$/.panic(...)` (#16888,
# EEL 0.3's "A proto pattern must have a {*} body"): the unit fails to compile
# with that message.

plan 3;

my $proc = run $*EXECUTABLE, '-It/lib', '-e',
    'use PatternDeclaratorSlang; class C { shout m { panic } }; say "ran"',
    :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
isnt $proc.exitcode, 0, 'the unit fails to compile';
like $err, /'cannot shout panic'/, 'with the message the action gave';
is $out, '', 'and runs nothing';
