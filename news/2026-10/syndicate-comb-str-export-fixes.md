# Syndicate: `.comb(rx//)`, exceptions from a user `.Str`, exported constants vs nested types

Three fixes found through the Syndicate distribution:

- **`.comb` accepts an `rx//` literal.** `rx/a/` and `rx:i/A/` build a regex
  value that carries its adverbs inline in the pattern (`":i A"`). `.comb`'s
  interpreter path only knew a bare `/.../`, so it searched for the regex's
  stringified form and found nothing. Syndicate::Discovery's
  `my constant $base-tag = rx:i/.../` made `base-url` return no URL.
- **An exception from a user `Stringy`/`Str` propagates out of `~$obj` and
  `"{$obj}"`.** Both paths swallowed every error and rendered the default
  `Type<id>` form. Now only a missing method falls back to that form. Syndicate's
  `~$feed` dies on a timestamp-less Atom feed, and the test checks for that
  graceful error.
- **An exported `my constant` beats a same-named nested type.** Inside
  `unit class Syndicate`, `my constant Atom is export` was imported as the
  class `Syndicate::Atom`, because the export lookup read the qualified env key
  first, and that key is the nested type's own binding. A type's self-binding
  is now consulted only after every store a constant can live in.

Syndicate's audit-round tests 31, 32 and 33 now pass. `t/35-convert` gets from
87 to 186 of 195; the rest is #11385, a class-scoped `my constant` passed
directly as a call argument.
