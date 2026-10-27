# Red: `*.attr ~= ...` WhateverCode and hyphenated same-name types as role arguments

`*.name ~= "!"` now primes the whole read-modify-write expression into a
one-argument WhateverCode (a `*` in a compound-assign target counted for nothing
before), and an `is rw` accessor Proxy on a type object is assigned through.
A second type minted under an already used hyphenated name no longer fails as a
role argument.  Both were found with the Red 0.2.7 suite (`t/18-phasers`,
`t/100-single-relationship-create`).

An imported `postfix:<++>` candidate (exported through an `EXPORT` map) is now
chosen for a Proxy operand such as `Model.column++`: the operand is ranked by
what its FETCH answers, not by the Proxy itself.
