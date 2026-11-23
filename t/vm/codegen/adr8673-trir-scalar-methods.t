# A TRIR `MethodGen` site whose receiver is an immutable scalar (`Str`,
# `Int`, `Num`, `Bool`) takes the pure native lane the VM's `CallMethod`
# takes for it, with the same guards; anything else keeps the general
# dispatch. The fixture's transcript must be the same with TRIR on and off,
# and it is pinned against rakudo's.
use Test;

plan 4;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-scalar-methods.raku').Str;

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
is $off-out, $on-out, 'TRIR off agrees';

# Checked against rakudo 2026.11.
is $on-out, q:to/END/, 'the transcript is what rakudo prints';
    numeric=12.5 1000 3.25
    failure=Failure handled=True
    methods=ABCAB 5 2
    junction=True True
    rendered=42
    END

# Not vacuous: the routines whose methods TRIR compiles ran as TRIR.
my @accepted = $on-err.lines.grep(*.contains(' accepted ')).map({ .words[1] });
ok @accepted.Set ⊇ <numeric count rendered>.Set,
    'numeric, count and rendered compiled to TRIR'
    or diag "accepted: @accepted[]";
