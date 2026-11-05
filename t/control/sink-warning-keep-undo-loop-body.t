use Test;

# A KEEP / UNDO phaser reads the iteration's value, so the last statement of a
# loop body that has one is not sunk: no "Useless use" warning (#16616).

sub err-of(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    $p.out.slurp(:close);
    $p.err.slurp(:close)
}

plan 6;

unlike err-of('for 1 { KEEP { say "K" }; say "body"; "ok" }'), /Useless/, 'KEEP: no warning for a string tail';
unlike err-of('for 1 { UNDO { say "U" }; say "body"; 5 }'), /Useless/, 'UNDO: no warning for an integer tail';
unlike err-of('while 1 { KEEP { say "K" }; last; "ok" }'), /Useless/, 'while body with KEEP';
like err-of('for 1 { say "body"; "ok" }'), /Useless/, 'a body without the phaser still warns';
like err-of('for 1 { LEAVE { say "L" }; say "body"; "ok" }'), /Useless/, 'LEAVE does not read the value: still warns';
like err-of('for 1 { KEEP { say "K" }; "ok"; say "body" }'), /Useless/, 'an earlier sunk statement still warns';

done-testing;
