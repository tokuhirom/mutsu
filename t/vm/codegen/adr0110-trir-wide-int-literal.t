# TRIR types an integer literal as a native `int` operand only while it fits
# in 32 bits, as rakudo does: `-> int $a { $a + 9223372036854775807 }`
# promotes to a big Int, where TRIR used to wrap it (#9234). A narrow literal
# or a native `int` operand wraps on BOTH paths; the untyped path used to
# promote (#9270).
use Test;

plan 4;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-wide-int-literal.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({});
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";
is $on-out, $off-out, 'TRIR and the untyped path agree';

# Checked against rakudo.
is $on-out, q:to/END/ x 2, 'the transcript is rakudo\'s';
    plus-max=9223372036854775808
    plus-wide=9223372039002259455
    max-plus=9223372036854775808
    minus-wide=-9223372039002259457
    times-wide=39614081257132168792477007872
    cmp-wide=lt ge
    plus-one=-9223372036854775808
    plus-param=-9223372036854775808
    times-three=9223372036854775805
    one-plus=-9223372036854775808
    minus-narrow=9223372034707292161
    into-native=-9223372036854775808
    END
