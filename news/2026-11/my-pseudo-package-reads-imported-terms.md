# `MY::NAME` reads a term an import brought in

`MY::ENOENT` (or `UNIT::NAME`) died with "Could not find symbol '&ENOENT' in 'GLOBAL::MY'" when
`ENOENT` was an enum value or a constant a `use` imported: the pseudo-package prefix was dropped
only for types and packages. It now reads the bare name as a term the scope holds. Universal::Errno's
`t/01-basic.t6` goes from 6 to 7 of 7 assertions (#16562).
