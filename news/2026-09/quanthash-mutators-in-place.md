# SetHash/BagHash/MixHash mutators work through attributes and accessors

`SetHash.set`/`.unset` and the `SetHash`/`BagHash`/`MixHash` `.grab`/`.grabpairs`
used to build a fresh QuantHash and re-bind the invocant's *variable name*. That
only reached a plain lexical: `$!q.unset("x")` in a method wrote to an env key no
attribute read consults (the change was not even visible later in the same
method), and `$obj.q.grab` had no name at all and died with "No such method
'grab'". App::Lorea's `Backlog` queue — a `has SetHash $!queue` drained with
`$!queue.grab` — therefore never emptied and the command reran forever (#9609).

The mutators now live in one place, `builtins::quanthash_mutators`, and mutate
the QuantHash's shared node in place — the mechanism `BagHash.add`/`.remove`
already used — so every holder (a variable, an alias, an attribute, an accessor
result, an array or hash element) observes the change. Both VM method-call
opcodes route to it, and the runtime's rebind-based copies are gone.

In-place mutation is only sound if a mutable QuantHash never shares its node with
an immutable one, and several coercions did exactly that (`$sethash.Set`,
`$set.SetHash`, `$bag.BagHash`, `my %h is SetHash = $set`, the set operators'
result re-flagging). They all go through the new
`Value::quanthash_with_mutability`, which keeps the node when the flag is
unchanged and hands back an unshared node when it flips.
