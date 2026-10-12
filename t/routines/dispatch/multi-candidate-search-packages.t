use Test;

# A bare-name multi call searches the current package, then each enclosing
# package, then GLOBAL, and stops at the innermost package that declares its
# own `proto` for the name (`candidate_search_packages`). With no `proto`
# anywhere the walk is the memoized package chain itself (#10111). These pin
# both shapes.

plan 6;

module Outer {
    multi sub greet(Int $x) is export { "outer-int" }
    module Inner {
        our sub call-int() { greet(1) }
        our sub call-str() { my $s = "a"; (try greet($s)) // "no-candidate" }
    }
}

is Outer::Inner::call-int(), 'outer-int', 'an enclosing package multi is found with no proto anywhere';
is Outer::Inner::call-str(), 'no-candidate', 'a call no enclosing candidate accepts finds none';

module Shadow {
    proto sub pick-one($) {*}
    multi sub pick-one(Int $x) { "shadow" }
    our sub go() { pick-one(5) }
}
proto sub pick-one($) {*}
multi sub pick-one(Int $x) { "mainline" }

is Shadow::go(), 'shadow', 'a package that owns a proto stops the walk at itself';
is pick-one(5), 'mainline', 'the mainline family keeps its own candidate';

multi sub late(Int $x) { "global-int" }
module Late {
    our sub go() { late(1) }
}
is Late::go(), 'global-int', 'GLOBAL candidates are reached from a nested package';

{
    multi sub infix:<⊕>(Int $a, Int $b) { $a + $b + 1 }
    my $acc = 0;
    $acc = $acc ⊕ $_ for 1..10;
    is $acc, 65, 'a user operator multi resolves on every iteration';
}
