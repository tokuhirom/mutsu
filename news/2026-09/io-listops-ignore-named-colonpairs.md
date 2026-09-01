# IO listops now keep named colonpairs out of output

`say`, `print`, `put`, and `note` now handle colonpairs written directly in
their argument list as named arguments.  They evaluate the pair value but do
not stringify the pair into the output, matching Raku:

```raku
say :debug, 'ready';       # ready
say (debug => True), 'x';  # debug => Truex
```

The grouped Pair remains positional data.  This preserves ADR-0021's rule
that namedness comes from call-site syntax, not from Pair data.

The compiler mints the existing named-argument marker for direct colonpairs
and bareword fat-arrow pairs in statement-form IO listops.  Their shared VM
argument flattening then excludes marked pairs before rendering, including a
directly slipped Pair (`say |(:debug), 'ready'`).

`t/io-listops-named-colonpairs.t` covers all four listops, grouped Pairs, a
slipped Pair, and an ordinary named call as the control case.
