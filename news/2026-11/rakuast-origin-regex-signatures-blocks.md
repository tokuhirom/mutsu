# RakuAST origins inside regexes: signatures, code blocks, colon arguments

The last four files of the `.origin` differential for #15780
(`rakuast-grammar-argument-bindings.t`, `rakuast-regex-literal-hash-index.t`,
`rakuast-regex-variable-alias-gap.t`, `rakuast-type-parameterized-ctor.t`)
are now identical to rakudo 2026.11. This slice placed:

- the signature of a `token` / `regex` / `rule` (`token word($value)`; an
  empty `rule r()` sits at its `)`), and `visit-children` of a regex
  declaration visits its signature even when none was written, as rakudo's;
- the code block of a predicate assertion (`<?{ so $<word> }>`, `<!{ ... }>`)
  and of a block quantifier (`\d ** {$n}`), its statements placed by
  re-parsing the code as a fragment;
- colon-form call arguments (`<.record: "seen $<word>">`) and a `%h<key>`
  call argument (`<word(%h<key>)>`);
- `$<word>` and `$0` interpolated into a string, and `$<word>`'s word quote
  between its brackets anywhere;
- a variable in a string followed by `'` (`"of '$src' is"`);
- the atom an interpolated variable is aliased to (`$foo = [<two>]`), over
  which rakudo's `Interpolation` runs;
- a single-quoted regex literal with a backslash (`'a("\$b")'`).

The regex declaration code moved out of `regex_origins.rs` into
`regex_decl_origins.rs`, and code blocks have their own
`regex_block_origins.rs`. Found on the way and filed: #18326 (`"$don't"`
interpolates `$don`).
