# A negated `<!:Prop>` assertion needs a character to look at

The ticket blamed the `:Script<...>` *parameterisation*: "`<!:L>` already works,
only the script-parameterized form misbehaves". Measuring against `raku`
v2026.06 showed `:Script<...>` was never the variable. `<!:L>` was equally wrong,
and the shared bug is about **end of string**:

| pattern on `'333'` | raku | mutsu (before) |
|---|---|---|
| `^^ \d+ <!:L>` | `｢33｣` | `｢333｣` |
| `^^ \d+ <!:Script<Tamil>>` | `｢33｣` | `｢333｣` |
| `^^ \d+ <!:Script<Latin>>` | `｢33｣` | `｢333｣` |
| `^^ \d+ <!:L>` on `'33a'` | `｢3｣` | `｢3｣` (already right) |

`<!:Prop>` is zero-width — `'33a' ~~ /^^ \d+ <!:L>/` is `｢3｣`, not `｢33｣`, so it
does not consume — but Rakudo's character-class matcher **bounds-checks before
it applies the negation**. With no character left the test fails whichever
polarity is asked for, which is why the greedy `\d+` has to give a character
back. mutsu returned `Some(pos)` at end of input whenever `negated` was set, so
the assertion never constrained anything and `\d+` kept the whole run.

The property itself was fine: `<:Script<Latin>>`, `<-:Script<Latin>>` and
`.comb(/<:Script<Latin>>/)` all already agreed with raku.

Three more divergences in the same family surfaced while establishing that
boundary, and are fixed with it:

- `<!:!L>` / `<?:!L>` — the *inner* `!` was folded into the property name, so
  `check_unicode_property("!L", c)` simply always said no. `<?:!L>` is now the
  positive assertion of the negated property ("there is a character and it is
  not a letter") and `<!:!L>` negates that again.
- `<!+alpha>` / `<!-alpha>` had no parse path at all and failed everywhere.
  Unlike `<!:Prop>`, an *enumerated* class negation is an ordinary zero-width
  lookaround in Rakudo — `<![abc]>` already behaved that way in mutsu — so it
  succeeds at end of string. They now compile to a negated `Lookaround` over
  the combined class, which is the `<![abc]>` treatment generalised.

Pinned by `t/regex-engine-gaps.t`, which records the full end-of-string table
above (including the `<![...]>` / `<!+alpha>` / `<!alpha>` rows that *do*
succeed there) so the two behaviours cannot be conflated again.
