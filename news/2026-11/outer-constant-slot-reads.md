# A sub reads an enclosing scope's constant once per call

A sigil-less constant declared in an enclosing scope -- a module's
`my constant VOID-ELEMENTS = <...>.Set` read from its subs -- was resolved by
name on every read, through the same by-name chain as any free variable:
MCP::Server::Tool::Web's `tokenize` looks three of them up for every start
tag it sees.

The read now goes through a hidden local of the reading frame
(`GetLexFree` with the `Constant` kind): the first read of a call resolves
the term by name and keeps the answer, and every later read of the call loads
it. A constant is never rebound, so the first answer is every later one. A
loop reading such a constant went from 9.4k to 7.0k instructions per
iteration, against 6.7k for the same constant declared in the sub itself.
