# A custom `is` trait on a class nested in a routine runs once, at compile time

A `trait_mod:<is>` applied to an `our` class declared inside a routine used to run on every
call of the routine. It now runs once, from the class's compile-time shell, seeing the unit's
lexicals in their static state, as in Rakudo (#12708).
