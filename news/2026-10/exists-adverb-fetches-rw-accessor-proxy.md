# `:exists` on an `is rw` accessor's result probes what the Proxy fetches

`X.accessor<k>:exists` with an `is rw` accessor answering a Proxy (Red's JSON
column, `.json-column<bla>:exists`) probed the Proxy itself and reported every
key absent, so Red's t/87-json-operations key-existence assertions failed even
after the `Bool` of an `EXISTS-KEY` answer was honoured. The `ExistsIndexAdv`
op now FETCHes a Proxy receiver first, as the `:delete` path already does.
