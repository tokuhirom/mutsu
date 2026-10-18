# Container type objects refuse `pop`, `set`, `STORE` and kin as Rakudo does

`Array.pop`, `SetHash.set`, `BagHash.add`, `Array.STORE`, `Array.splice` and the quant hashes'
`grab` raised `X::Method::NotFound` on a type object. They now raise Rakudo's errors (#13881).
An `only` method (`pop`, `shift`, `set`, `unset`, `add`, `remove`) raises
`X::Parameter::InvalidConcreteness`. A multi (`splice`, `grab`, `STORE`) raises `X::Multi::NoMatch`
naming the type object.

The growing methods still vivify a type object a variable holds: `my Array $a; $a.push(1)`.
