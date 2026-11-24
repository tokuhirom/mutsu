# `$m[$i]` on a lazy Match skips the general subscript preamble

Reading a positional capture by index (`$/[0]`, `$m[1]`) from a lazy Match,
a regex match or a grammar cursor in an action method, used to walk the general
subscript path before it reached the Match arm. On the way it probed the
receiver for an `is Array` parent, a `Sequence` role with its own iterator, an
enum aggregate and a user `postcircumfix`.

The subscript's fast path now answers a non-negative `Int` index on a lazy Match
directly. It reads the slot from the node's memoized capture list, which is the
list the general path reads, so a repeated read still hands out the same child
value. It leaves the read to the general path when a user
`postcircumfix:<[ ]>` may answer, and for a grammar whose class does `Sequence`.

Found through Cro::Uri's action methods (#18416).
