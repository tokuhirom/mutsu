# Hash `:exists` autothreads Junction keys

An existence check with a Junction key now checks each eigenstate and returns
the Bool dictated by the Junction kind. This applies to ordinary and object
hashes, including negated `:exists`. Previously the Junction was treated as a
single key, so an existing key could appear absent.

This resolves #9786.
