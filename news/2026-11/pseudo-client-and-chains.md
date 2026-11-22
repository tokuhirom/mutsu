# `CLIENT::` reports the calling unit's revision and package; pseudo chains miss quietly

`CLIENT::LEXICAL::<CORE-SETTING-REV>`, `CLIENT::CORE::<CORE-SETTING-REV>` and
`CLIENT::LEXICAL::<$?PACKAGE>` now name the client of the running code. The client is
the nearest calling frame declared in another unit, so a 6.d module's routine called
from a 6.e script reports `e` and `GLOBAL`. A 6.e routine called back from that module
reports `d` and the module's package. A light call pushes no routine frame, so the walk
skips the running unit's frames by their declaring file. A `.map` block, which pushes no
frame at all, is not seen yet (#18845).

A bare word behind two or more pseudo-packages (`CORE::UNIT::True`,
`CORE::CALLERS::CORE::True`, a hundred `OUTER::`s) now reads the symbol through the
pseudo-stash chain. It misses quietly instead of dying with "Could not find symbol",
which is roast pseudo-6e's "no guts spillage" subtest (#18742).
