# Set/Bag/Mix constructors and unique/repeated/squish honour a user-defined WHICH

`set(...)`, `bag(...)`, `mix(...)`, `unique`, `unique(...)`, `repeated` and `squish`
compared elements by object id and ignored a class's own `WHICH`. They now resolve the
user `WHICH` first (the same warm-then-key mechanism `.Set` and `===` already used), so
value-type classes following the `ValueObjAt` idiom dedupe as in Rakudo.
