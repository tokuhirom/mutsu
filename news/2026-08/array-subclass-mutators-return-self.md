# Array subclass mutators now return the invocant

Direct `push`, `append`, `prepend`, and `unshift` calls on an `is Array` subclass now return the
subclass instance itself, matching Rakudo identity and type semantics. Previously the VM correctly
updated the instance's backing storage but returned the raw `Array` storage value produced by the
native mutator helper.

The array-backed-instance fast path now reuses the freshly rebuilt instance returned by storage
write-back for these four self-returning methods. Other mutators, such as `pop` and `shift`, retain
their element return values. A TAP regression covers identity, subclass type preservation, and
storage mutation for all four methods.
