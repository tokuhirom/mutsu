# HomoGlypher now runs through anonymous-sub `samewith` recursion

HomoGlypher 1.8.7 moved from partial to green in the ecosystem sweep. Its
`unwind`, `collapse`, and tokenization helpers use `samewith` from anonymous
`sub` closures to recurse with new arguments; mutsu now preserves the callable
dispatch context for those routines, including its garbage-collector roots.

The behavior is covered by a focused `samewith` regression test and the
distribution's five test files now pass with 34 of 34 assertions.
