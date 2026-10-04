# Regex source trees: five more forms

When the source-regex parser (`src/regex_tree.rs`) cannot model some part
of a regex, it gives up on the whole declaration. RakuAST then refuses the
declaration as "regex declaration without a source tree", which was the most
common `.AST` refusal left: 51 `t/` files. A survey of the patterns it gave
up on found several small gaps, each now modelled the way rakudo 2026.09
renders it:

- **Whitespace before `*`, `+` or `?`** (`<w> +%% ";"`). Only a spaced `**`
  was accepted before. rakudo wraps the atom in `WithWhitespace`, as it
  already did for `**`.
- **A quantified separator** (`<w>+ % \s+`). The separator took a single
  atom.
- **`<?>` and `<!>`**, rakudo's `Regex::Assertion::Pass` / `Fail`. These are
  new tree nodes and RakuAST classes.
- **A leading `|` or `||`** (`[ | a | b ]`, and a grammar's one-branch-per-line
  layout). It opens no empty branch, and rakudo drops it.
- **`[\ ]` in a character class**: an escaped whitespace character is that
  character. Outside a class it stays the "unspace" error rakudo reports.

The `.AST` text of each form matches rakudo's. Because these patterns now
have a source tree, they run through the tree-based execution path, so the
behaviour was checked under plain `mutsu` and `MUTSU_RAKUAST=1` against
`raku`.

Code-carrying forms stay out of this change. A declaration's code blocks
lose their source text when the tree is lowered (#11923), so the `** { … }`
block quantifier and `:my` declarations would hit the same problem.
