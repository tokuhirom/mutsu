# RakuAST: substitutions, match variables and more regex trees

Substitutions, transliterations, match variables and a batch of regex
constructs read back as the nodes rakudo has (measured on rakudo 2026.09), and
the lowering rebuilds what the parser builds, so the round trip is the parsed
program.

- **`s///`, `S///`, `ss///`, `tr///`, `TR///`.** A `Substitution` over the
  pattern's regex tree (no `QuotedRegex` around it) and a replacement
  `QuotedString` (or, for `s[...] = EXPR`, an `Assignment` infix and the
  expression); a `Transliteration` over its two `QuotedString` sides. Their
  adverbs are colonpairs in the order written: `ColonPair::True` for a flag,
  `ColonPair::Number` for `:2nth`, `ColonPair::Value` for `:x(2)`,
  `:nth(1,3)` and `:x(1..3)` (`src/rakuast/substitution.rs`). The parser now
  carries the pattern's source tree and the written adverbs beside what it
  executes (`Expr::Subst::tree`, `Expr::Transliterate::adverbs`), and lowering
  derives the executable form from them again through the parser's own adverb
  routine (`parser::subst_pattern_source`), so a substitution lowered from a
  tree cannot read an adverb differently from a parsed one. The replacement
  lowers to the per-match thunk of `s[...] = EXPR` (mutsu has no `qq` source
  to give back).
- **Regex adverbs with arguments.** `m:x(2)/a/`, `m:2nth/a/` and the other
  written adverbs are colonpairs on the `QuotedRegex`, and its execution value
  is built by the same parser routine
  (`parser::regex_execution_value`) instead of a second copy of the adverb
  table in `lower.rs`.
- **Match variables.** `$<name>` is `Var::NamedCapture`, `$0` (also inside a
  string, where the parser reads `$/[0]`) `Var::PositionalCapture`.
- **Regex constructs.** A new `regex_tree::RegexExtension` holds the
  constructs only the RakuAST boundary reads (the matcher keeps its text
  path): `<?alpha>` / `<!ww>` / `<?.name>` / `<?[x]>` lookaheads,
  `< a b >` word lists, `A ~ B C` (`Regex::Nested`), `:my $x = 1;` statements
  (`Regex::Statement`), `$0` / `$<name>` back-references, `<~~>`, `a:` /
  `a:!` / `a:?` (`BacktrackModifiedAtom`) and `"x $y z"` interpolating quotes.
  A code block, a statement, a `** { ... }` block range
  (`Quantifier::BlockRange`) and an interpolating quote keep their spelling in
  the hidden `source` field of `regex_code.rs`. A property takes its predicate
  (`<:Script<Latin>>`, `<:Nv(1)>`), and a class names a rule with a hyphen
  (`<+name-sep>`: the scanner used to split it into `name` and `-sep`, which
  the engine then followed until the stack overflowed).
- `RakuAST::*` accessors that share a name with a `Cool` method
  (`Substitution.samespace`) are no longer refused on a node.
- A single processor prints as `("words",)`, as rakudo's one-element list.

Found on the way, filed separately: `$s ~~ S/b/X/` answers `False` and
`$s ~~ TR/a/o/` rewrites `$s` (#12174); a class naming an undeclared rule
overflows the engine's stack (#12175).

Left for later in the plan (not S8): `s[a] = "lit"` renders without its
`infix` (the parser keeps a literal right-hand side as the quote form),
`<|b>` / `<|w>` (rakudo's tree for them is not the node they execute as),
`|||`, `$(...)` / `@(...)` interpolations, `<rx=$x>`, a curly-quoted term,
`Regex::Block` over a `:my` in a nested group, `%h<x>` inside a quote
(rakudo: `LiteralHashIndex`), `Heredoc` terminators — S9/S10.

New tests: `t/rakuast/rakuast-substitution.t` (63),
`t/rakuast/rakuast-regex-assertions-and-backrefs.t` (48) and
`t/rakuast/rakuast-regex-blocks-properties-quotes.t` (38), whose tree parts
also run under `raku`. Slice S8 of #7564; closes #11923.
