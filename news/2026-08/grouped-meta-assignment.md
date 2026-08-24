# Fix grouped list meta-assignment

Grouped scalar declarations now produce a List value while keeping their variables in the surrounding lexical scope. Cross and zip assignment meta-operators also write updated values back through literal lists of scalar containers, fixing the register add-back used by the Rosetta Code MD4 implementation.
