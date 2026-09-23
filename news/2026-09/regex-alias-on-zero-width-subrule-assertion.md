# An alias on a `<?subrule>` assertion is a capturing subrule call

`$<a>=<?foo>`, `$<a>=<?before x>` and `$<a>=<!before y>` produced a single `a`
capture in mutsu, keeping the assertion zero-width. Rakudo does something else,
and it is observable in `.keys`, `.hash` and `.caps` (issue #9112, witnessed by
`CSS::Module::CSS3::Selectors`, whose `negation-expr` has a `before` key from
`$<nested>=<?before [:i':not(']>`).

The rule comes straight from the QAST (`raku --target=ast`): NQP's
`metachar:sym<var>` action calls `subrule_alias` on any subrule-typed atom,
which renames it `a=foo` and resets its subtype to `capture`, overwriting the
`zerowidth` subtype that the `?`/`!` prefix had set. So:

- `$<a>=<?foo>` is exactly `$<a>=<foo>`: it consumes, and captures both `a`
  and `foo`. (In the issue's table `$<a>=<?foo> x` printed `()` because the
  parse *failed*, not because nothing was captured.)
- `$<a>=<?before x>` is `$<a>=<before x>`: `before` is zero-width by itself, so
  both `a` and `before` hold an empty match.
- `$<a>=<!foo>` / `$<a>=<!before y>` keep the `negate` flag on a capturing
  call. When the subrule fails the cursor moves to the failed match's negative
  position, so the overall match can never succeed. mutsu models that as an
  always-failing atom.
- A non-subrule assertion (`<?[x]>`, `<?{ ... }>`, `<?:L>`) becomes an ordinary
  subcapture and stays zero-width, which mutsu already did.

Both regex parsers needed the change. The structured `regex_tree` turns an
aliased positive `Lookaround` into a capturing `NamedLookaround`, and its
alias lowering now moves the atom's own capture name into
`secondary_named_capture` instead of overwriting it. Before, even
`$<a>=<before x>` lost its `before` key. The legacy parser drops the `?` of an
aliased `<?name …>` and forces an aliased `<!name …>` to fail. It also gives
the bare `<before …>` / `<after …>` spelling its `before` / `after` capture, the
same as the tree parser.

Pinned by `t/regex/regex-alias-zero-width-assertion.t`.
