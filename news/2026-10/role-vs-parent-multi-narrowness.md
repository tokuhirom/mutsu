# A composed role ties a parent class in multi dispatch; `.does` takes a value's type

Multi dispatch scored a role the argument's class composes at the end of the class MRO (distance
6 for `class Dict is Hash does TH`), so `multi m(Hash $d is raw, |c)` beat `multi m(TH $h)` — and
`is raw`/`is rw` was read as narrowness between parameters whose declared types are unrelated.
Rakudo ties such candidates (the slurpy one loses). A composed role is now as near as a direct
parent, and `is rw`/`is raw` only breaks a tie between equal declared types. This is what lets
`PDF::COS::Tie::Hash.COERCE` mix a role into the dictionary itself instead of a copy
(`t/cos-tie-entry.t` in PDF).

`Mu.does` also treats a plain value as its type (`2.does(2)` is True), as Test's `does-ok($x, 2)` needs.
