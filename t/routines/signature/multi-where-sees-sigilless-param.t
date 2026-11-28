use Test;

# #19344: a `where` clause of a multi candidate reads an earlier sigilless
# parameter (`Int \k where * == n`), as the single-routine binder does.
plan 8;

multi S2 (Int \n where * > 0, 0) { 'zero' }
multi S2 (Int \n, Int \k where * == n) { 'diag' }
multi S2 (Int \n, Int \k) { 'other' }

is S2(3, 3), 'diag', 'the where clause sees n';
is S2(3, 2), 'other', 'and rejects a different value';
is S2(5, 0), 'zero', 'an earlier candidate still wins';
is S2(0, 0), 'diag', 'zero equals zero';

multi A (Int $n, Int $k where * == $n) { 'same' }
multi A (Int $n, Int $k) { 'diff' }
is A(3, 3), 'same', 'the same with a sigilled parameter';

sub C (Int \n, Int $k where * == n) { 'ok' }
is C(3, 3), 'ok', 'a plain sub sees it';
throws-like { C(3, 2) }, X::TypeCheck::Binding::Parameter, 'and refuses a different value';

my @warnings;
{
    CONTROL { when CX::Warn { @warnings.push: .message; .resume } }
    S2(4, 4);
}
is @warnings.elems, 0, 'no uninitialized-value warning while dispatching';
