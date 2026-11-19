# `GLOBAL::<$G>++` creates and increments `$GLOBAL::G`

A package stash's `$` entry is the package variable, but two things got in
the way. A stash read (`GLOBAL::<$G>`) did not see a value the stash
assignment had created: the assignment stores `GLOBAL::G` without a sigil,
and the stash listed an uppercase-leading sigil-less key as the term `G`
rather than the scalar `$G`. And `GLOBAL::<$H>++` bound the stash entry as a
temporary, which for a variable that did not exist yet held no container, so
the increment was lost.

A package-qualified `$` store (`SetGlobal` of `Pkg::name`) now records its key
as a scalar in the `our` table, as an `our $Baz` declaration already does
(#17364), and `++`/`--` on a package stash's literal `$` entry take the
read-increment-write path the dynamic and lexical pseudo-stashes use since
#18120 (#18127).
