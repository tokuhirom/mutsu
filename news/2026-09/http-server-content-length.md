# HTTP::Server::Tiny can consume Content-Length request bodies

`HTTP::Server::Tiny` stores a small `Content-Length` request body in
`IO::Blob`. A package-qualified class such as `IO::Blob` must still resolve the
bare `Blob` attribute type to the core `Blob`, and the module's `Supply` method
uses `$*DEFAULT-READ-ELEMS` for its default chunk size. mutsu now handles both
settings correctly, so an `HTTP::UserAgent` POST reaches the PSGI app with its
body intact instead of failing during request handling.

The regression is pinned by `t/io/io-blob-content-length.t`.

Closes #8672.
