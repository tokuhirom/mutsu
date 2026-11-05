# `prompt` answers `val($line)` and Nil at end of input

`prompt` returned a plain Str for every line, so `prompt("n: ").^name` was `Str` for `1234`
(rakudo `IntStr`, `0x1f` `IntStr`, `1.5` `RatStr`). It now runs the line through `val`, and at
end of input answers `Nil` (as `get` does) instead of the `Any` type object. Found with
Prompt::Hidden (t/01-plain.t).
