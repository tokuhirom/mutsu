# Type captures shadow quote languages in routine bodies

Signature type captures are now registered in the body parser's lexical scope.
This lets a captured `S` be read as a type term in `S.^name`, rather than as the
start of an `S` substitution that consumes later method calls. It fixes parsing
of chained calls after routines with such captures.
