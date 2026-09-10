use v6;
use Test;

# Two properties of a `where` constraint during multi dispatch:
#
#  (1) A `where` block is ordinary code. An exception it throws propagates out
#      of the whole dispatch -- it is NOT "this candidate does not match", so a
#      later candidate must not silently take over.
#  (2) An unsupplied parameter that has a DEFAULT is checked against that
#      default's value, so a multi can select a candidate purely on its
#      defaulted parameter (including a default that reads `$.attr`).

plan 14;

# --- (1) an exception from a `where` propagates ------------------------------

sub check($x) { die "boom" if $x eq 'bad'; True }
multi sub f(@s where { check(@s[0]) }) { 'matched' }
multi sub f(@s)                        { 'fallback' }

is f(['good']), 'matched', 'a where that returns True still selects its candidate';
throws-like { f(['bad']) }, X::AdHoc, message => /boom/,
    'an exception thrown by a where clause propagates out of the dispatch';

my class X::WhereBoom is Exception {
    method message { 'where-boom' }
}
multi sub g($x where { $x == 2 ?? die(X::WhereBoom.new) !! True }) { 'first' }
multi sub g($x)                                                    { 'second' }
is g(1), 'first', 'the non-throwing case still dispatches normally';
is g(3), 'first', 'and keeps dispatching normally afterwards';
throws-like { g(2) }, X::WhereBoom,
    'a typed exception from a where keeps its type';

# A `where` that merely returns False is still just "no match".
multi sub h($x where { $x > 10 }) { 'big' }
multi sub h($x)                   { 'small' }
is h(20), 'big', 'a true where matches';
is h(2), 'small', 'a false where falls through to the next candidate';

# --- (2) a defaulted parameter's `where` selects the candidate ---------------

my $tag = 'B';
multi sub d(Str:D $x where { $_ eq 'A' } = $tag) { 'sub-A' }
multi sub d(Str:D $x where { $_ eq 'B' } = $tag) { 'sub-B' }
is d(), 'sub-B', 'an unsupplied defaulted param is matched against its default';
is d('A'), 'sub-A', 'an explicitly supplied argument still wins';

class Tagged {
    has Str:D $.tag is required;
    multi method m(Str:D $t where { $_ eq 'A' } = $.tag) { 'method-A' }
    multi method m(Str:D $t where { $_ eq 'B' } = $.tag) { 'method-B' }
}
is Tagged.new(:tag<A>).m, 'method-A', 'a default reading $.attr selects the A candidate';
is Tagged.new(:tag<B>).m, 'method-B', 'a default reading $.attr selects the B candidate';
is Tagged.new(:tag<A>).m('B'), 'method-B', 'an explicit argument overrides the attribute default';

multi sub n($x where { $_ > 10 } = 42) { 'big' }
multi sub n($x where { $_ <= 10 } = 42) { 'small' }
is n(), 'big', 'a numeric defaulted where picks the matching candidate';
is n(3), 'small', 'and a supplied argument is matched on its own';

done-testing;
