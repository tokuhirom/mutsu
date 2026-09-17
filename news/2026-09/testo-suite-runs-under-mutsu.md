# Testo 1.003009 runs under mutsu

The Testo 1.003009 test suite now passes under mutsu, including nested `group`
callbacks and `is-run` command checks. The interpreter now:

- recognizes callable leaves nested inside Pair sub-signatures during multi
  dispatch;
- binds the implicit topic while evaluating declaration-time attribute
  `where` expressions such as `where .so`; and
- avoids treating an inherited same-named `&` parameter as a callback's lexical
  override when the callback did not capture it.

The extracted suite matches Rakudo: all six test files pass.
