# An anonymous `token`/`regex`/`rule` term can carry a signature

Rakudo's `regex_def` is `<deflongname>? <signature>? '{' <p6regex> '}'`: the name
and the signature are optional and independent of each other. mutsu's term
parser accepted only the shortest of the four spellings — a `{` had to follow
the keyword — so an anonymous declarator with parameters was a hard parse
error:

```
$ mutsu -e 'my $t = token ( $x ) { \d+ }; say $t.^name'
===SORRY!=== Error while compiling -e
Confused. expected statement: ...
```

That is the whole public API of `HomoGlypher`, whose `tokenize` method returns
exactly such a term and whose callers invoke it as `<&tokenized: 'bar'>`.

## The signature had to go somewhere

Parsing the signature is the easy half. The term's result is a Regex *value*,
and a regex is a routine — so the parameters have to ride on the value: there
is no `token_defs` entry to look them up on, the way the named declarator's
`Stmt::TokenDecl { params, param_defs }` is looked up when the rule is called.
Accepting the signature and dropping it would have turned a parse error into a
regex that silently ignores its argument, which is worse.

`RegexClosure` — the payload of `Value::RegexCaptured`, already the carrier for
a regex's captured defining scope — grew a `signature` field, and
`Value::regex_with_signature` builds one. `capture_regex_closure` preserves it
when the compiler attaches the defining scope, so a declarator that both closes
over a lexical and takes a parameter keeps both.

## Calling it

Three things had to come together for `<&tokenized: 'bar'>` to actually work,
and none of them existed:

- **A lexical holding a Regex was not a subrule target at all.** `<&t>`,
  `<&t(...)>`, `<&t: ...>` and `<&$t(...)>` all went to the rule registry,
  found nothing, and quietly failed to match. `regex_lexical_ref.rs` is the
  fallback the subrule dispatcher consults when the registry lookup comes up
  empty: it resolves the binding the reference actually spells (`<&x>` is the
  `&x` routine lexical, `<&$x>` the `$x` scalar — raku rejects crossing them),
  binds the call's arguments to the value's signature, and hands back an
  instantiated pattern in the same shape the named path produces. The
  resolution reads the caller's lexical scope, so it deliberately runs ahead of
  the subrule memos, which are keyed by name alone.
- **The value's defining scope was not installed for the match.** A regex is a
  closure over the scope it was written in, and an anonymous declarator
  returned from a sub reads that scope from its code blocks, which run inline
  in whatever env the cursor reaches them with.
  `install_subrule_dynamic_params` — which already establishes a rule's `$*`
  parameters for exactly that window — now installs the lexical's captured
  scope too, on the same save/restore list.
- **A parameter was invisible inside a `<?{ … }>` code assertion.** The pass
  that bakes bound parameters into a pattern's code blocks treated every `<…>`
  construct as opaque except a lookaround, so a code assertion's body never saw
  them. This was broken for *named* rules too — `token t($x) { <?{ $x eq 'a' }> }`
  saw `Nil` where raku sees the bound value — and the fix applies to both.

## Two flavour bugs fixed on the way

Building the pattern from an anonymous body previously stopped at
normalization, while the named declarator went on to inject `rule`'s implicit
`<.ws>` and the ratchet prefix that `token` and `rule` carry. So an anonymous
`rule` did not skip whitespace and an anonymous `token` did not ratchet. Both
now go through the same finalizer.
