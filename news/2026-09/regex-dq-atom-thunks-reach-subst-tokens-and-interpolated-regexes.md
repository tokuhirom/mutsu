# `"..."` regex atoms interpolate in `s///`, tokens and `<$re>`

A double-quoted regex atom such as `"x @a[0]"` follows qq-string rules: its
subscripts, method calls and blocks interpolate, and the result matches
literally. [#9628](https://github.com/tokuhirom/mutsu/issues/9628) made this
work for regex literals by compiling each such atom to a qq closure (a
"thunk") that runs when the regex's scope is installed for a match. Three
other places still read the atom as plain text
([#9673](https://github.com/tokuhirom/mutsu/issues/9673)):

```raku
my @a = <p q>;
my $s = "x p y"; $s ~~ s/"x @a[0]"/Z/; say $s;        # Z y
grammar G { token TOP { "@a[0]" } }; say so G.parse("p"); # True
my $re = /"@a[1]"/; say so "q" ~~ /<$re>/;             # True
```

Each place now gets the thunks:

- **`s///` and `S///`.** The substitution ops carry the pattern's thunk
  slots. The thunks are installed around the substitution, as a regex
  literal's scope is installed around `~~`.
- **`token`, `rule` and `regex` declarations.** The declaration captures its
  thunks on its body's regex value. A class or role body has no frame, so the
  thunks are built by a declaration-time chunk instead. A grammar resolves a
  rule by name rather than matching that value, so the thunks run in the
  rule's resolve-and-match window, where its `$*` parameters are bound. Their
  results are then spliced into the rule's pattern as it is parsed.
- **`<$re>`.** An interpolated regex's scope is installed only when its atom
  is matched, after the pattern has been parsed. For this case there is a new
  match-time atom, `RegexAtom::QqInterp`. It reads the thunk's result from
  the installed scope and matches it literally. The same atom covers a rule
  body that is parsed for a static analysis outside its window. The
  analyses (LTM, prefilter, call graph) treat the atom as opaque, as Rakudo's
  NFA treats the code it compiles the atom to.

A token's body regex value now carries closures in its scope. That exposed an
AST probe, `body_reads_args_array`, that formatted the whole body with
`Debug`, captured closures included, and its memory use blew up. The probe now
skips literal-value statements, which cannot read `@_`.
