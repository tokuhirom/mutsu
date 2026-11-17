# A sigil alias names the whole interpolated `$var`

`$<a>=$var`, `@<a>=$var` and `$0=$var` now behave as in rakudo, whose
`subrule_alias` renames the interpolation node itself (#14510).

On the match-time path, an aliased `$var` is lowered to a call of the
variable's value (`regex_alias_var_call`). The value is read when the cursor
reaches it. A `Regex` value is called as itself, so its nested captures are
filed under the alias. Any other value matches as the literal its
interpolation would match, under the atom's `:i`/`:m`. A quantifier repeats
the call, so `$<a>=$s+` gives one Match per iteration for a Str value too, and
`@<a>=$s` gives a List.

The paths that still splice the value into the pattern text (`s///`,
`.match`, `.subst`, `m:i`) bracket a spliced value that follows a sigil
alias. Before, a multi-character Str gave the alias only its first character:
`s/$<a>=$t/[$<a>]/` with `$t = "abc"` produced `x[a]`.
