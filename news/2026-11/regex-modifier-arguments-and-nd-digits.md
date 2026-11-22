# Regex: `:i(0)`/`:1i`, `*:!`, a leading `&` and Nd digits; rx.t runs again

`roast/S05-mass/rx.t` had stopped compiling. The RakuAST finalizer refused every
regex literal whose source tree it could not build ("literal Regex without a
declarator tree"), and some spellings had no tree node (#18713):

- an internal modifier with an integer argument, in either spelling
  (`:i(0)`, `:1i`, `:ignorecase(1)`, `:s(0)`). A zero switches the mode off, as
  rakudo shows with `negated => True`. A negated adverb takes no argument:
  `:!i(1)` is `:!i` followed by the capture `(1)`;
- a colon greed modifier after a quantifier when a letter or digit follows
  (`xa*:!y`, `a**:!2..4`);
- a leading `&` in a branch (`/a |& b/`, `/& a/`), which rakudo drops as it
  drops a leading `|`;
- the decimal digits of any script in a backreference or a `**` count
  (`$٠` is `$0`, `a**۳` is `a**3`).

The regex matcher handled the same spellings wrongly. A leading modifier was
peeled by its own hand-written list, which knew no arguments. Now the leading
list and a modifier written later in the pattern go through the same routine,
and the runtime parser takes the colon greed spelling and Nd digits too. `.AST`
of each construct matches rakudo's, and `rx.t` runs all 756 subtests with 14
failing (it ran none before). The remaining failures are filed as #18736,
#18737, #18738 and #18739.
