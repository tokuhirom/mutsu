# ADR-0110: the native int and num ops of a TRIR routine answer what the
# untyped path and rakudo answer, including for negative operands.
#
# Pinned by this file: `nqp::div_i` floors (-17 div 5 is -4; TRIR once
# truncated to -3), and Raku's `%` takes the divisor's sign (-17 % 5 is 3;
# TRIR once lowered it to `nqp::mod_i`, which takes the dividend's and
# answered -2). `nqp::mod_i` itself keeps the dividend's sign.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-int-ops.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_TRIR_DUMP => '1' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";
is $on-out, $off-out, 'TRIR and the untyped path agree';

# Checked against rakudo.
is $on-out, q:to/END/, 'the transcript is rakudo\'s';
    int-ops=-308645 250
    cmp-ops=35 26 44
    wraps=-2 -9223372036854775805
    num-ops=2.671875 0.9375
    num-cmp=1 2 4
    sized=4400300 25499999 6374464
    tri=55 500500
    short=5 3 43
    bump=7 7
    bump-twice=11 11
    caller-slot=9020
    skip-ws=4 4
    divide=3 -4
    divide-by-zero=died
    int-ops=-308645 250
    cmp-ops=35 26 44
    wraps=-2 -9223372036854775805
    num-ops=2.671875 0.9375
    num-cmp=1 2 4
    sized=4400300 25499999 6374464
    tri=55 500500
    short=5 3 43
    bump=7 7
    bump-twice=11 11
    caller-slot=9020
    skip-ws=4 4
    divide=3 -4
    divide-by-zero=died
    raku-mod=3 -3 2
    nqp-mod=-2 2 2
    raku-mod=3 -3 2
    nqp-mod=-2 2 2
    END

# `raku-mod` is declined on purpose: TRIR has no op with `%`'s semantics.
my @routines = <int-ops cmp-ops wraps num-ops num-cmp sized tri short bump
    bump-twice caller-slot skip-ws divide nqp-mod>;
my @accepted = $on-err.lines.map({ m/^ 'trir: ' (\S+) ' accepted'/ ?? ~$0 !! Empty }).grep(* (elem) @routines).unique;
is-deeply @accepted.sort.List, @routines.sort.List, 'every routine but raku-mod is accepted into TRIR'
    or diag $on-err;
