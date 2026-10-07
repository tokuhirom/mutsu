# The receiver-mutating built-in methods are rows of the method table

ADR-11276 slice 3F. A built-in method that writes through its receiver is now one registered
row of the method table, like every other built-in method, and the one handler behind the
row answers every kind of receiver that used to have its own copy.

The new row kind, `Handler::Mut`, takes the receiver's *place*: a named binding (`@a`,
`$r` holding an array, `%h`, an attribute) with the VM chunk that mirrors it, or a detached
container (`f().pop`, the backing storage of an `is Array` instance, the array a mixin
wraps). It is registered by its owner only and reached through one entry, `invoke_mut`,
so no shape lookup, call-site lane or pure entry can run it, and the debug cross-check
never repeats the mutation. The name stays what keys the declared element type, `is default`
and a compunit's or an `our` package array; container identity already made the write itself
land in the shared node.

Rows registered: `Array` and `List`'s `push`, `append`, `unshift`, `prepend`, `pop` and `shift`
(a `List` refuses them with `X::Immutable`), `Array.splice` and `Array.grab`, `Hash.push` and
`append`, `BagHash.add` and `remove`, `SetHash.set` and `unset`, `grab` and `grabpairs` on
`SetHash`, `BagHash` and `MixHash` (and the immutable `Set`, `Bag` and `Mix`, which refuse
them), and `Str.subst-mutate` and `substr-rw`. The array mutators were written six times
(the `@` and the sigil-less arms of the by-name entry, the VM's two fast paths, the
`is Array` storage helper and the by-value block), `Hash.push` three times; they are one
implementation now, and the cascade arms, `try_native_array_mut`, `try_native_array_splice`,
`try_native_hash_mut_bound`, `array_mutate_copy`, `array_grab` and `vm_baghash_mutators.rs`
are gone.

Behaviour changes toward Rakudo, each pinned in a focused test:

- `MixHash.grab` is refused (".grab is not supported on a MixHash"); `grabpairs` works.
- `set` and `unset` on a user subclass of `SetHash` reach the wrapped storage.
- An undeclared named argument is ignored by every one of these methods
  (`@a.pop(:zzz)`, `%h.push(:zzz)`, `$set.grab(:zzz)`).
- `Nil` pushed onto a scalar that holds an array decays to the element default, as it does for
  an `@` variable.
- A by-value `QuantHash` receiver (`f().unset('x')`, `$obj.q.grab`) is mutated in place; the
  slow path used to work on a copy.
- An `augment` or `.wrap` of the receiver's type that defines the method takes the call from a
  row (it was skipped on the VM's early paths).
