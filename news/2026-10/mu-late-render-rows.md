# Mu's last-resort methods and the multi-index positional protocol are rows

The last arms of the method cascade — `Mu`'s `gist`, `raku`, `perl`, `DUMP`, `isa`, `REPR`, `Str`, `Stringy`,
`Int`, `Numeric`, `Real` and `Cool`'s `EVAL`, and the `AT-POS`/`EXISTS-POS`/`ASSIGN-POS`/`BIND-POS`/`DELETE-POS`
calls with several indices — are now late rows of the method table. The cascade finds them by walking the
receiver's MRO (ADR-11276 §9.138).

Because the walk follows the MRO, `EVAL` belongs to `Cool`, as in Rakudo. A `Blob` is not `Cool`, so
`Blob.new(49).EVAL` is now "No such method", where it used to evaluate the bytes.
