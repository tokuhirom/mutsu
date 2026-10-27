use Test;

# The MUTSU_RAKUAST round-trip frontend mode (ADR-10723 Stage 0,
# src/rakuast/frontend.rs). mutsu-only: the variable means nothing to raku.
#
# The hash alias now round-trips too (#14350). The no-fallback guarantee is
# pinned by a converter-level test in src/rakuast/frontend.rs, independent of
# whichever source construct the next slice makes representable.

plan 8;

sub run-mutsu(Str $mode, *@args) {
    my %env = %*ENV;
    if $mode { %env<MUTSU_RAKUAST> = $mode } else { %env<MUTSU_RAKUAST>:delete }
    my $proc = run $*EXECUTABLE, |@args, :%env, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my $plain = q[{ say "blk" }; say 1 + 2; sub f($x) { $x * 2 }; say f(21)];
{
    my ($rc, $out) = run-mutsu('1', '-e', $plain);
    is $rc, 0, 'a program that round-trips runs in the mode';
    is $out, "blk\n3\n42\n", 'and prints what it prints without the mode';
}

my $hash_alias = q[my $x = " a"; $x ~~ m/%<chars>=( \s+ \S+ )+/; say 1];
{
    my ($rc, $out, $err) = run-mutsu('1', '-e', $hash_alias);
    is $rc, 0, 'a hash capture alias now round-trips in the mode';
    is $out, "1\n", 'the round-tripped program reaches the following statement';
    is $err, '', 'the supported alias raises no refusal';
}

{
    my ($rc, $out) = run-mutsu('', '-e', $hash_alias);
    is $out, "1\n", 'without the variable the same program runs the same';
}

{
    my ($rc, $out, $err) = run-mutsu('1', '-e',
        'say EVAL q[my $x = " a"; $x ~~ m/%<chars>=( \s+ \S+ )+/; 1]');
    is $out, "1\n", 'an EVAL string is a unit the mode covers';
}

{
    my ($rc, $out) = run-mutsu('1', '-e', 'use Test; plan 1; ok 1, "module loaded"');
    is $rc, 0, 'a used module round-trips and loads';
}
