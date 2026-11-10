# Core-type return constraints with a smiley skip registry probes

A routine declared `--> Bool:D` / `--> Int:D` paid two registry walks on every call
(`is_definite_return_spec` and the owner type-name resolution) before its return check.
Core types with a `:D`/`:U` smiley now decide both from the name alone, cutting such a
call's cost by about half. Found while profiling MCP::Server::Tool::Web's HTML extractor
(`t/12-extract.rakutest`, 63 s to 53 s; the remainder is tracked separately).
