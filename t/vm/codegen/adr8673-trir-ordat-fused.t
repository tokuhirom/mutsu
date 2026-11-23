# `nqp::ordat` at a native slot's position (`OrdAtLocalSlot`) and through an
# `int $pos is rw` reference (`OrdAtLocalRef`), the fused forms the TRIR
# peephole makes of `LoadI`/`GetRefI` + `OrdAtLocal`. An inlined callee's
# reference form becomes the caller's slot form. The fixture's transcript
# must be the same with TRIR on and off, and it is pinned against rakudo's.
use Test;

plan 4;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-ordat-fused.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_TRIR_DUMP => 'ops' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-out, $on-out, 'TRIR off agrees';

# Checked against rakudo 2026.11.
is $on-out, q:to/END/, 'the transcript is what rakudo prints';
    skipped=4
    first-word=3 0 4
    codes=-1,104,233,108,108,111,-1
    END

# Not vacuous: all three shapes were compiled.
ok $on-err.contains('OrdAtLocalRef') && $on-err.contains('OrdAtLocalSlot')
    && $on-err.contains('InlineEnter'),
    'the reference, slot and inlined forms all ran';
