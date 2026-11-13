# A CHECK-time pass can resolve and rewrite the unit tree like Red's `red-compile`

Red's optional query compiler runs as `CHECK red-compile` over `$*CU`: it walks
the unit's RakuAST, translates the `.grep`/`.map` callbacks it can, and rewrites
those calls in the tree rakudo then compiles. mutsu now supports the resolver
API that pass drives (#17501):

- `$*CU.resolver.clone` keeps a scope stack: `push-scope($scope)`,
  `pop-scope`, and `resolve-lexical('$name')`, which answers the declaration
  in the innermost pushed scope (a block's `my` or parameter, the unit's own
  subs and packages), an imported routine's `Declaration::Import`, or the
  setting's routine or type.
- `IMPL-BEGIN` / `IMPL-CHECK` on a node built at CHECK time resolve its
  lookups against the pushed scopes, so its `Var::Lexical` answers
  `is-resolved` and `resolution`.
- `ApplyPostfix` / `ApplyInfix` / `ApplyPrefix` gained `set-operands`.

A rewrite made by a CHECK phaser now reaches execution: the unit's tree is
fingerprinted when the CHECK phase begins (the end of the BEGIN prologue, or
when `$*CU` is first built after it), and a tree that differs once the CHECK
phase ends is the one the unit runs, as it already was for `add-check-code`
closures. The fingerprint is now a structural hash of node classes, fields and
literals; it used to be the tree's `Debug` text, which rendered every resolved
declaration's closure with its whole environment and ran Red's t/106 out of
memory.

Red's `t/106-compiled-regex.rakutest` now matches rakudo (33/33).

The same work brought several related fixes:

- `RakuAST::Regex::Assertion::Named(::Args).get-implicit-lookups` answers the
  `&name` lookup of `<name>`, resolved to a `my regex` / `token` / `rule` in
  scope (a builtin rule's lookup stays unresolved). `Str.AST` records these
  lookups too, and `QuotedRegex.compile-time-value` declares the rules they
  resolved to ahead of the regex, so `<pair>` calls the lexical `pair`. An
  `rx//` quoted regex has a compile-time value as well.
- `Parameter.signature-constraint` answers the `Signature` type object.
- `<.at(N)>`, `<?at(N)>` and `<!at(N)>` work, not only `<at(N)>`.
- A `multi infix:<~>` (or `&&`, `||`, `^^`, `//`) with no precedence trait
  keeps the core operator's precedence. It used to sit at the additive level,
  so after `use Red` the expression `'b' x 3 ~ 'a'` parsed as
  `'b' x (3 ~ 'a')`.
- When a unit runs its CHECK-rewritten tree, the program that tree replaced
  no longer runs its own unit-level LEAVE phasers and pre-installed END
  phasers as well. Each now runs once, from the tree.

Red's t/110, t/112, t/114 and t/115 now match rakudo too.
