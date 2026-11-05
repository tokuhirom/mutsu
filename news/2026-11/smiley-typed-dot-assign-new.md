# `my Int:D \b .= new: 42` constructs `Int`, not `Int:D`

A `.= new` initializer on a smiley-constrained sigilless variable or constant
(`my Int:D \b .= new: 42`, `my Int:D constant \a .= new: 42`) called `new` on the
type name with its definiteness smiley attached and died with "You cannot create
an instance of this type (Int:D)". The smiley qualifies the variable's
constraint, not the constructed type, so it is dropped from the call target. This
gets `roast/MISC/misc.t` subtest 6 past its `.= new:` lines (#16437); the next
failure there is the typed `&` variable (`my Int:D &foo = sub (--> Int:D) {}`).
