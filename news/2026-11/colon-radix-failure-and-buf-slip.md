# `:8($s)` fails softly and `Buf.Slip` slips its elements

Two bugs found with Archive::Tar::PP, whose six test files now match rakudo (68/68):

- `:N("body")` threw "Cannot parse '' as base 8" on a malformed or empty body. Rakudo
  returns a lazy `X::Str::Numeric` Failure, so `:8($size-field) // 0` falls back to 0.
  mutsu now returns that Failure, with rakudo's message wording.
- `Buf.Slip` (and `blob8.Slip`) produced a one-element Slip holding the Buf. A Buf is
  Positional, so it now slips its elements, which is what the common padding idiom
  `$val .= new(Buf.allocate($n, 0).Slip, $val.values.Slip)` relies on.
