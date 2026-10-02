use Test;

# The MUTSU_RAKUAST round-trip frontend mode (ADR-10723 Stage 0,
# src/rakuast/frontend.rs). mutsu-only: the variable means nothing to raku.
#
# The refusal cases use an attribute with a trait (`has $.x is rw`), which the
# converter does not model yet. When it starts to round-trip, swap in another
# construct the converter refuses: what is pinned here is that a refusal is an
# error in the mode, never a silent fallback.

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

my $refused = q[class C { has $.x is rw }; say 1];
{
    my ($rc, $out, $err) = run-mutsu('1', '-e', $refused);
    isnt $rc, 0, 'a construct the converter refuses fails the program';
    like $err, /'MUTSU_RAKUAST'/, 'and says the mode refused it';
    is $out, '', 'nothing ran: no fallback to the parser tree';
}

{
    my ($rc, $out) = run-mutsu('', '-e', $refused);
    is $out, "1\n", 'without the mode the same program runs normally';
}

{
    my ($rc, $out, $err) = run-mutsu('1', '-e', 'say EVAL q[class D { has $.x is rw }; 1]');
    like $err, /'MUTSU_RAKUAST'/, 'an EVAL string is a unit the mode covers';
}

{
    my ($rc, $out) = run-mutsu('1', '-e', 'use Test; plan 1; ok 1, "module loaded"');
    is $rc, 0, 'MUTSU_RAKUAST=1 leaves used modules alone';
}
