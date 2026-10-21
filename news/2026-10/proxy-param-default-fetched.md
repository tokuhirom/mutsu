# A Proxy default binds the value it fetches

A readonly parameter whose default evaluates to a Proxy used to bind the Proxy
itself. One way to get such a default is to call an `is rw` accessor that
returns a Proxy, as in `method decode($encoded = $.encoded)`. Because the
parameter held the Proxy, multi dispatch on it saw "Any:D" instead of the
Blob the Proxy holds, and found no candidate. A supplied Proxy argument was
already fetched, so the default now gets the same treatment: a readonly
parameter binds the fetched value, and an `is raw` one keeps the container.

PDF's `PDF::COS::Stream.decode($encoded = $.encoded)` forwards the default to
`PDF::IO::Filter.decode`, which dispatches on `Blob`/`Str`. Before this
change, decoding an object stream died with "Cannot resolve caller decode"
(#12907).
