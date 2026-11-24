# A call made inside a `for` loop no longer hashes the topic twice

A `for` loop marks its topic `$_` read-only. Every routine or method call
clears that mark before binding its own `$_`, and the call's return puts it
back. Both went through the hash map of read-only names. The topic's mark now
lives in its own field next to that map, so both are a plain field write.

A method call plus a sub call per iteration of a `for` loop went from 15.34k
to 15.13k instructions per iteration (callgrind slope, profiling build).
