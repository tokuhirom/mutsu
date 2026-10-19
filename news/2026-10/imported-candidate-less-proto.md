# An imported candidate-less proto is the routine it is

A `proto` with no candidates whose body answers the call itself is an
ordinary routine (#10696). Importing one did not work (#13994):

- `use M; pa(1)` was "Unknown function", for a `unit module` and a
  bare-file module alike: the import found no routine entry for the proto
  and installed only the proto table's alias, which a call does not run.
- `{ use M; pa(1) }`, where the importing program has a `sub pa` of its
  own, called the program's routine. The `unit module` export's fallback
  to `GLOBAL::pa` picked up the importer's routine as the module's.

The import now hands the proto's definition on as a plain routine. A
block-scoped `use` binds it in the block's lexical import set
(ADR-8751), where it wins over the importer's routine and a closure keeps
it. An imported only-star proto binds there too, so it no longer hides the
importer's routine of the name from a callee written outside the block.
The `GLOBAL::name` fallback leaves out the importer's own routines.

Left: a block-scoped `use` of a bare-file module still finds nothing for a
candidate-less proto. Its only home is `GLOBAL`, which the BEGIN-time
preload scope rolls back.
