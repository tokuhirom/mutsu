# Slang::Roman, Slang::NumberBase and Slang::Date run on the slang host

Three gaps stood between the slang host (ADR-15762) and the distributions that
add a `number`/`value` production:

- The host's grammar and actions classes were named `Mutsu::Slang::Host::...`.
  Slangs tell the RakuAST frontend from the legacy one with
  `self.^name.starts-with('Raku::')`, so they took the legacy `QAST` branch and
  died. The classes are now `Raku::Grammar` and `Raku::Actions`.
- `RakuAST::Literal.from-value($v)` is implemented: it builds the Int, Str, Num
  or Rat literal node for the value's own type.
- An `exit` inside a `CATCH` handler without `when`/`default` ended with the
  in-flight exception reported and status 1; the program now exits with the
  status `exit` asked for, as rakudo does.

Slang::Roman (48 of 48), Slang::NumberBase (4 of 4) and Slang::Date (2 of 2,
status 0) now match rakudo on their own test files.
