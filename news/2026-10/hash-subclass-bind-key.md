# `BIND-KEY` on a Hash subclass instance

`self.BIND-KEY($k, $v)` inside a `class H is Hash` (or `%h.BIND-KEY` on a
`my %h is H`) died with "No such method 'BIND-KEY' for invocant of type
'Hash'". The subclass delegates Associative methods to its backing hash, but
the native `BIND-KEY` lives only on the call opcode path, which that by-value
delegation never reaches. The delegate now binds the key into the backing hash
in place, as it already did for `STORE`, so every holder of the instance sees
it.

WriteOnceHash's `STORE` binds its initial pairs this way. That distribution
still fails afterwards on the role-body ordering problem tracked in #11628.
