# Rainbow's tokenizer matches rakudo: lookarounds re-read their variables, code blocks find their own end

Rainbow 0.4.4's own suites (`t/tokenize.rakutest`, `t/sub-langs.rakutest`) now
pass in full under mutsu. Before, `tokenize` was killed partway (0/3 reported)
and `sub-langs` failed one of four. Three regex bugs were behind it (#13928).

**A lookaround re-reads its variables.** A variable inside a lookaround
(`<!before $*closer>`, `<?after $x>`) is interpolated when the lookaround body
is parsed, a step below the enclosing pattern. The enclosing pattern's own
interpolation copies `<...>` through untouched, so its text, the memo key of
its parse, did not change with the variable. The tree built for the first
value was then reused for every later one. In a loop,
`/ <?before $c> . /` kept matching the first `$c`. In a grammar, a lookahead
read `$*closer` as it stood outside the rule that declared it. A
sub-pattern parse that substitutes a value now marks the enclosing parse as
not a function of its text. The sub-pattern itself is still memoized under
its interpolated text.

**A bare `@name` in a lookaround matches its elements literally**, as it does
outside one, and the `@*name` twigil interpolates too. A lookaround used to
rewrite `@f` to `<@f>`, which reads the elements as regexes: `@f = '|'` died
with "Null regex not allowed". The rewrite existed only to dodge the stale
memo above, so it is gone.

**One scanner finds the end of a regex code block.** `{ … }`, `<?{ … }>`,
`<{ … }>` and the brace search of the interpolation pass had three separate
copies of the brace count, and only one skipped `#` comments. A
`# Don't …` comment in a rule's code block opened a string that never closed,
and the block silently never ran. They now share
`regex_code_nested::code_block_body_len`. That scanner skips comments, nested
regex literals and quoted strings, and treats an apostrophe inside an
identifier (`don't`) as part of the name. The parser's scanner for regex
literals learned the identifier rule too.
