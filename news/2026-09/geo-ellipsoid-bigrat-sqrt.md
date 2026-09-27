# Geo::Ellipsoid reaches full ecosystem parity

Geo::Ellipsoid 1.0.1 is now green: all 11 baseline test files and all 3,877
baseline assertions pass under mutsu and Rakudo.

The remaining failures came from the free `sqrt` routine returning `NaN` when
decimal arithmetic promoted the squared displacement to a `BigRat`. Numeric
square roots now share one implementation across the routine and method paths,
including `BigRat` conversion, with a regression test covering the free
function form.
