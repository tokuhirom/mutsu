use Test;

# An inline map/grep/first callback returns the value of its final output
# statement, even when it reads the topicalized element and calls a sub first.
# These outputs agree with Rakudo 2026.11.
plan 5;

sub is-run-output($code, $expected, $desc) {
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    is $out, $expected, "$desc (exit {$proc.exitcode}, stderr $err)";
}

is-run-output 'sub f {}; my @r = (1, 2).map({ f(); say $_ }); say @r',
    "1\n2\n[True True]\n", 'say is the map callback result';

is-run-output 'my @r = (1, 2).map({ put $_ }); say @r',
    "1\n2\n[True True]\n", 'put is the map callback result';

is-run-output 'my @r = (1, 2).map({ print $_ }); say @r',
    "12[True True]\n", 'print is the map callback result';

is-run-output 'my @r = (0, 1).grep({ say $_ }); say @r',
    "0\n1\n[0 1]\n", 'say is a true grep predicate';

is-run-output 'my $first = (0, 1).first({ say $_ }); say $first',
    "0\n0\n", 'say is a true first predicate';
