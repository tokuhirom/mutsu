# Private typed object hashes accept set-union values

mutsu now separates the value and key constraints when writing a set-union
result into a private typed object-hash attribute. Red's relationship registry
can therefore use `%!relationships ∪= $attribute` without rejecting the
`Bool` set-membership value, while preserving the `Attribute` object key.
