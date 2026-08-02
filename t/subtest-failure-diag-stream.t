use Test;

# Rakudo splits the two TAP diagnostic streams by whether an assertion was
# TODO'd, not by how deeply nested it is: a TODO'd failure's `# Failed test ...`
# goes to `$todo_output` (stdout, so it stays inside the subtest's TAP block)
# and a real failure's goes to `$failure_output` (stderr), indented to its
# subtest level. A failing subtest also closes with its own
# `# You failed N tests of M` on stderr.
#
# mutsu keyed the choice on `subtest_depth() == 0`, so every in-subtest failure
# diagnostic landed on stdout, and it emitted each stderr diagnostic twice (once
# at the raise, once when the buffered stderr was flushed at exit).

plan 6;

sub streams-of(Str:D $code) {
    my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($out, $err);
}

my ($out, $err) = streams-of 'use Test; plan 1; subtest "foos" => { todo 1; ok 0; ok 0 }';

is $out, join("\n",
        '1..1',
        '# Subtest: foos',
        '    not ok 1 -  # TODO 1',
        '    # Failed test at -e line 1',
        '    not ok 2 - ',
        '    1..2',
        'not ok 1 - foos',
    ) ~ "\n", 'a subtest keeps only the TODO\'d diagnostic on stdout';

ok $err.contains("    # Failed test at -e line 1\n"),
    'the real failure\'s diagnostic goes to stderr, indented to its subtest level';
ok $err.contains("    # You failed 1 test of 2\n"),
    'the failing subtest closes with its own count on stderr';
is +$err.comb("# Failed test 'foos'"), 1,
    'the outer failure diagnostic is emitted exactly once';

# A top-level failure keeps its diagnostic on stderr and out of the TAP stream.
($out, $err) = streams-of 'use Test; plan 2; ok 0, "a"; ok 1, "b"';
is $out, "1..2\nnot ok 1 - a\nok 2 - b\n", 'a top-level failure leaves stdout as pure TAP';
is +$err.comb('# Failed test'), 1, '... and reports it once on stderr';
