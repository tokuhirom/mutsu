use Test;

# A multi method's `where` clauses run the way rakudo's dispatcher runs them:
# candidates are ranked on their nominal signature, and a candidate's `where`
# is checked only when the walk reaches it, so the clauses of candidates
# ranked below the winner never run. A deferral (`callsame`) checks the next
# candidates when it resumes, inside the callee. Found in Red, whose AST
# optimizer evaluated every candidate's `where compare(...)` on every call.

plan 8;

my @log;
sub chk($tag, $verdict) { @log.push: $tag; $verdict }

class P {
    multi method m(Int $x where chk('p', True)) { 'p' }
    multi method m(Int $x where chk('q', True)) { 'q' }
}
is P.m(1), 'p', 'the first constrained candidate that binds wins';
is @log.join(','), 'p', 'a later candidate\'s where never runs';

@log = ();
class O {
    multi method opt(Int $a, Int $b where chk('a', False), 1) { 'a' }
    multi method opt(Int $a, Int $b where chk('b', False), 1) { 'b' }
    multi method opt(Int $a, Int $b, 1) { 'plain' }
    multi method opt(Str $a, Str $b where chk('s', False), 1) { 's' }
    multi method opt($a, $b, $c) { 'any' }
}
is O.opt(1, 2, 1), 'plain', 'failed constraints fall through to the plain candidate';
is @log.join(','), 'a,b', 'only the nominally matching constraints ran';
@log = ();
is O.opt("x", "y", 2), 'any', 'a literal mismatch is part of the trial bind';
is @log.join(','), 's', 'the where before the literal still ran, as in rakudo';

@log = ();
class Q {
    multi method m(Int $x where chk('x', True)) { my $*D = 5; nextsame }
    multi method m(Int $x where { chk('y', ($*D // 0) == 5) }) { 'saw dyn' }
    multi method m(Int $x) { 'no dyn' }
}
is Q.m(1), 'saw dyn', 'a deferral checks the next where inside the callee';

@log = ();
class R {
    multi method m(Int $x where chk('r1', True)) { 'r1,' ~ callsame }
    multi method m(Int $x where chk('r2', False)) { 'r2' }
    multi method m(Int $x) { 'r3' }
}
is R.m(1), 'r1,r3', 'callsame skips a candidate whose where fails';

