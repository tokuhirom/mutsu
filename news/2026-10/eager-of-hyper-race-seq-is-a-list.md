# `eager` of a HyperSeq or RaceSeq yields a List

The `eager` statement prefix returned a `.hyper` / `.race` sequence unchanged,
so `eager race @buffer.STORE: ...` compared unequal to the `List` it should
be (`eqv` checks the type). It now drains the sequence into a `List`, like
`.eager`. This was the second blocker of the `annotations` distribution's
`t/01-containers` Buffer subtest.
