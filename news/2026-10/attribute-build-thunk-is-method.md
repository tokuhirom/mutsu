# `Attribute.build` hands out a `Method`

The thunk `Attribute.build` returns for a non-constant default (and so the `400`
payload of `.^BUILDPLAN`) was a `Sub`; Rakudo's is a `Method`. It is now tagged
`Method`, which closes the payload-type remainder of #14260.
