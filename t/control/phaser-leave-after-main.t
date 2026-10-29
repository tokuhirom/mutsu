use Test;

plan 4;

# Reduced from Red's t/99 header (#14094): rakudo calls `sub MAIN` from the end
# of the mainline, so the file's own LEAVE phasers run after MAIN, not before.
sub run-it(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    $p.out.slurp(:close).lines.join('|');
}

is run-it('LEAVE say "LEAVE"; sub MAIN(Str $x = "a") { say "in MAIN $x" }; END say "END";'),
    'in MAIN a|LEAVE|END', 'file-level LEAVE runs after MAIN, before END';

is run-it('LEAVE say "L1"; LEAVE say "L2"; sub MAIN { say "main" }'),
    'main|L2|L1', 'several LEAVE phasers run last-declared first';

is run-it('my $x = 5; LEAVE say "x=$x"; sub MAIN { $x = 6; say "main" }'),
    'main|x=6', 'LEAVE sees the lexicals MAIN changed';

is run-it('LEAVE say "LEAVE"; say "mainline"'),
    'mainline|LEAVE', 'without MAIN the LEAVE still runs at the end of the mainline';
