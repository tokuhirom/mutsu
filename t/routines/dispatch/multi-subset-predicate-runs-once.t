use Test;

# #10986: a multi winner's subset / `where` predicate runs once per call --
# the dispatch match that picked the candidate is the check, the binder does
# not re-run the user's code.

plan 9;

my $c = 0;
subset P of Int where { $c++; True };

multi sub f(P $x) { 'P' }
multi sub f(Str $x) { 'Str' }

is f(1), 'P', 'subset-typed multi sub candidate wins';
is $c, 1, 'its subset predicate ran once';
f(1);
is $c, 2, 'once again on the next call';

class D {
    multi method mm(P $x) { 'P' }
    multi method mm(Str $x) { 'Str' }
    multi method wm($x where { $c++; True }) { 'where' }
    multi method wm(Str $x) { 'Str' }
}

$c = 0;
is D.mm(1), 'P', 'subset-typed multi method candidate wins';
D.new.mm(1);
is $c, 2, 'a multi method runs its subset predicate once per call';

$c = 0;
is D.wm(1), 'where', 'where-constrained multi method candidate wins';
D.new.wm(1);
is $c, 2, 'a multi method runs its where clause once per call';

# A predicate that rejects still rejects: the losing candidate is skipped and
# the call fails when nothing else binds.
subset Even of Int where * %% 2;
multi sub g(Even $x) { 'even' }
multi sub g(Str $x) { 'str' }
throws-like { g(3) }, X::Multi::NoMatch, 'a failing subset predicate still excludes the candidate';

class E {
    multi method m(Even $x) { 'even' }
    multi method m(Str $x) { 'str' }
}
throws-like { E.m(3) }, X::Multi::NoMatch, 'and for a multi method too';
