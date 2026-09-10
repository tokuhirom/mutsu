unit module Issue7806Own;

# Deliberately empty: the test only cares whether this module's own bare
# package-name binding survives a first load that happens inside a sub call
# wrapping an EVAL (mirroring Test's `use-ok`, which is `EVAL ( "use $code" )`
# inside `try { }` inside `multi sub use-ok`).
