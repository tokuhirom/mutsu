# Binding a non-Positional to an `@` variable names the value

`my @s := <a b c>.Seq` and `my Int @z := (1,2).Seq` now report
`... but got Seq (("a", "b", "c").Seq)`, with the value's `.raku` after the type,
as rakudo does (found by the raku-doc differential sweep, `Language/list.rakudoc`).
