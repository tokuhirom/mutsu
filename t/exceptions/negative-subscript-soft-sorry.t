use Test;

# A negative literal subscript is a "sorry" of the grammar: the parse goes on,
# the unit's CHECK code still runs, and the compile phase then fails with the
# report (#15742). Without compile-phase code the program fails as before.
# Expected output is rakudo 2026.09's.

plan 12;

sub compile(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    ($proc.exitcode, $proc.out.slurp(:close), $proc.err.slurp(:close))
}

my ($code, $out, $err) = compile(q:to/RAKU/);
    CHECK { note "check ran" }
    my @a = 1, 2;
    say @a[-1];
    say "main ran";
    RAKU
is $code, 1, 'a negative subscript fails the compilation';
like $err, /'check ran'/, 'the CHECK phaser still ran';
like $err, /'===SORRY!=== Error while compiling -e'/, 'the report names the file';
like $err, /'Unsupported use of a negative -1 subscript'/, 'it says what is wrong';
like $err, /'at -e:3'/, 'it names the line';
like $err, /'say @a<HERE>[-1];'/, 'and the place';
is $out, '', 'the program did not run';

($code, $out, $err) = compile(q:to/RAKU/);
    my @a = 1, 2;
    say @a[-1];
    RAKU
is $code, 1, 'without a CHECK phaser it fails too';
like $err, /'Unsupported use of a negative -1 subscript'/, '... with the same complaint';
is $out, '', '... and does not run';

($code, $out, $err) = compile(q:to/RAKU/);
    CHECK { note "check ran" }
    my @a = 1, 2;
    say @a[0..-1];
    RAKU
is $code, 1, 'a range ending in a negative literal is refused too';
like $err, /'check ran'/, '... after the CHECK phaser';
