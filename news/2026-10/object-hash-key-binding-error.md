Object hashes now report a rejected key as
`X::TypeCheck::Binding::Parameter`, naming the `key` parameter and its
expected type. Stores, binds, reads, existence checks, deletion, slices, and
the explicit associative methods share the key parameter diagnostic. A
failure of the stored value's type remains an assignment error.
