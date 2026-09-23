# `&&` / `||` in a TRIR body yield an OPERAND, not its truth value.
#
# The short-circuit form narrowed every operand to its truthiness, so a boxed
# operand came back as `1`: `has-interp($s) && 'both'` answered `1`
# (t/modules/import-export/imported-call-expression-prefix.t, once the
# resolving call started entering TRIR; a resolution-cache hit had the same
# bug). Only native int operands, whose value is their truth, stay in TRIR,
# including an int-returning `nqp::` op reached through the generic op path.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo), the
# boxed forms decline, and the int forms are accepted.
use Test;

plan 6;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-short-circuit.raku').Str;

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
is $on-out, q:to/END/ x 2, 'the transcript carries the expected answers';
    boxed-and => both False
    boxed-or => True fallback
    int-and => 7 0
    int-or => 1 4
    END
my @lines = $on-err.lines;
ok @lines.first(* eq 'trir: boxed-and declined') && @lines.first(* eq 'trir: boxed-or declined'),
    'a boxed operand declines' or diag $on-err;
ok @lines.grep(/^ 'trir: int-' ['and'|'or'] ' accepted'/) == 2,
    'native int operands are accepted' or diag $on-err;
