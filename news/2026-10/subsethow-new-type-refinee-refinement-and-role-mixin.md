# `Metamodel::SubsetHOW.new_type` mints a real subset

`Metamodel::SubsetHOW.new_type(:name, :refinee, :refinement)` registers a subset (base type and
predicate) like a `subset ... of ... where ...` declaration, so `~~`, `.^refinee` and
`.^refinement` see it (it used to accept nothing). `(Metamodel::SubsetHOW but Role).new_type(...)`
keeps the role on the minted type's HOW, so `Perl6::Metamodel::SubsetHOW+{Role}` answers the role's
methods on `.^method` while the native metamethods still work (Red's `^submodel`, #15177).
