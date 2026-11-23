# JSON::Fast string decoding spends less per codepoint

`unjsonify-string`, the routine JSON::Fast decodes every escaped or
non-word string with, shifts each codepoint off a `Uni` and pushes it onto
another. Each of those native list ops first decoded the list value through
the general `view()` match to learn that it was a `Uni`. A bare `Uni` is now
recognized by its tag alone.

A generic `nqp::` op in a TRIR body with deferred routine-stack entries
(ADR-8673 D1) also asked, every time, whether it could observe the stack. It
did so by scanning a list of op ids and comparing the op's name. It now reads
a table indexed by the op id.

The 100-record SPDX decode went from 60.30M to 58.46M instructions (−3.0%,
callgrind).
