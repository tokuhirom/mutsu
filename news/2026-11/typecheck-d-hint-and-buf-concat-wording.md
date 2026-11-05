# `:D` assignment hint and the Blob `~` wording

Two message gaps found by the raku-doc differential (`Language/containers.rakudoc:373`,
`Language/contexts.rakudoc:147`):

- `my Int:D $d = 3; $d = Int` now ends with rakudo's "(perhaps Nil was assigned to a :D which had
  no default?)" on every assignment path; only the declaration path carried it before.
- `$blob ~ 1` / `$blob ~ []` name `'Str'` in `X::Buf::AsStr`, as rakudo does; `~` names `'Stringy'`
  only when the other operand is a Str.
