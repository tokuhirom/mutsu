# `use v6.x` must be the first statement, and `use v6c` means `use v6.c`

rakudo takes a language-version pragma only as the unit's first statement,
with comments and Pod allowed before it. Anywhere else, whether after another
statement, inside a block or after a `BEGIN`, it is a compile error,
`X::Language::TooLate`: "Too late to switch language version. Must be used as
the very first statement." mutsu accepted the pragma anywhere and switched
the version mid-unit. The dot after `v6` is optional in rakudo (`use v6c`),
and mutsu read that as 6.d.

The `use` parser now checks that only whitespace, comments and Pod precede
the pragma in the unit's source (`is_first_in_source`), and the version
normalizer accepts the dotless spelling. `-MModule` used to put its `use` in
front of the program text, which would now break `mutsu -MTest -e 'use v6.c;
...'`, so the `use` goes after a leading version pragma instead
(`mutsu::leading_version_pragma_end`). roast
`6.c/APPENDICES/A03-older-specs/01-misc.t` now passes its "language
switching" group.
