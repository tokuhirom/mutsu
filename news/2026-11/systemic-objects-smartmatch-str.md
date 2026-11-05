# `$*KERNEL ~~ 'linux'` matches: Systemic objects smartmatch a Str by their name

`Str.ACCEPTS(Any:D)` compares the object's stringification, but only the
classes with a hand-written arm (IO::Path, Date, Match, ...) were handled, so
the documented platform idiom `if $*KERNEL ~~ 'linux' { ... }` (and `$*PERL ~~
'Raku'`) was always False in mutsu. `Kernel`, `Distro`, `VM`, `Raku`, `Perl`
and `Compiler` now stringify through their `Str` row for a Str matcher. Found
by the raku-doc differential (`Type/Test.rakudoc`). The `Distro`/`VM` contents
(`name`, `version`) still differ from rakudo's and are tracked separately.
