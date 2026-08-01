unit module ShadowingTap;

# A module that exports routines named exactly like the Test module's, to pin
# that mutsu's *native* Test provider does not overrule an imported routine.
# Every routine here prefixes its output so the pin can tell which
# implementation answered the call. Used by t/test-fn-import-shadow.t.

my $count = 0;

multi sub plan($n) is export { say "MINE plan $n" }

multi sub ok(Mu $cond, $desc = '') is export {
    $count = $count + 1;
    say $cond ?? "MINE ok $count - $desc" !! "MINE not ok $count - $desc";
    ?$cond;
}

multi sub is(Mu $got, Mu $expected, $desc = '') is export {
    $count = $count + 1;
    say $got eqv $expected ?? "MINE ok $count - $desc" !! "MINE not ok $count - $desc";
}

sub diag($message) is export { say "MINE diag $message" }

sub done-testing() is export { say "MINE done, ran $count" }
