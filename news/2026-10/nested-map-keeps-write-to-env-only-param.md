# A nested map/grep/first block keeps its write to an env-only parameter

A block run by the inline `.map`/`.grep`/`.first` loop that assigned a captured variable
the consuming frame held as the same binding had that write undone when the loop put the
displaced env bindings back. A variable with no shared cell, such as a pointy-block or
`for` `is copy` parameter, therefore never saw it:
`(1,2).map(-> $a is copy { (0..1).map({ $a += 10 }); $a })` returned `(1 2)` instead of
`(21 22)`. The loop now skips the save/restore for a written capture whose binding is
identical to the consumer's, and still restores an unrelated same-named lexical.

Found with the `envy` distribution (`Envy::Util::CRC32`'s table builder), whose
`t/01-crc.rakutest` now passes 7/7 under mutsu. Locked on #10045.
