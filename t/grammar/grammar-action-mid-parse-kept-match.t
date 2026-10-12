use Test;

# A capture's Match that user code read and kept in mid-parse -- from a code
# block, or from a `.wrap`ped token's `callsame` -- was materialized before the
# parse stamped its grammar class on the run. The finished tree must not hand
# that stale Match to the action walk: the action's `$/` then read as a bare
# grammar instance (`G.new`) and `make` died with X::Make::MatchRequired.
# Values are rakudo's.

plan 4;

{
    my @keep;
    grammar G { token TOP { <num> { @keep.push: $<num> } }; token num { \d+ } }
    class A {
        method num($/) { make +$/ * 2 }
        method TOP($/) { make $<num>.made }
    }
    my $m = G.parse('5', :actions(A));
    is $m.made, 10, 'a capture kept by a code block still runs its action';
    is $m<num>.made, 10, 'and carries its made value';
}

{
    my @seen;
    grammar W { token TOP { <num> ' ' <num> }; token num { \d+ } }
    W.^find_method('num').wrap: sub (|c) { my $r := callsame; @seen.push: $r; $r };
    class B {
        method num($/) { make +$/ * 2 }
        method TOP($/) { make $<num>».made }
    }
    my $m = W.parse('5 6', :actions(B));
    is-deeply $m.made, [10, 12], 'a wrapped token whose result was kept runs its action';
    is @seen.elems, 2, 'the wrapper ran once per call';
}
