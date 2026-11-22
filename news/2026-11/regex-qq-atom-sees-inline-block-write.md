# A regex `"..."` atom sees what an earlier inline block of the same match wrote

`my $s = "a"; "xb" ~~ / x { $s = "b" } "$s" /` failed to match: the `"$s"`
atom is a compiled qq thunk that captured `$s` by value when the regex was
built, while the inline `{ $s = "b" }` block is evaluated by name, so the
capture analysis never saw the write and gave `$s` no shared cell. The
analysis now walks each regex literal's code blocks (`{ }`, `<?{ }>`,
`<{ }>`, quantifier blocks) and counts the scalars they name as mutated, so a
closure that captures one (the qq thunk above all) shares its cell and reads
the live value, as Rakudo does (#18630).
