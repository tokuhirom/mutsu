# Container binding failures carry rakudo's beginner hint

A typed or untyped `@`/`%` parameter, or a `$` parameter typed with a
Positional or Associative type, that rejects an untyped container now says
what rakudo says (#10815):

```
Type check failed in binding to parameter '@a'; expected Positional[Int] but got Array (["b"]). You have to pass an
explicitly typed array, not one that just might happen to contain
elements of the correct type.
```

The hint follows rakudo's `X::TypeCheck.explain`: a Positional expectation of
Arrays suggests "Did you mean to expect an array of Arrays?"; otherwise a
Positional or Associative expectation given an argument whose `.of` is `Mu`
(an untyped `Array`, `List`, `Hash`, `Map`, `Pair` or `Range`) is told to pass
an explicitly typed container. An Associative expectation names the argument
by its type alone, as rakudo's `gotn` does, for every parameter rather than
only `%`-sigiled ones.

The explanation is passed through a new `crate::word_wrap::naive_word_wrap`,
an exact reproduction of rakudo's `Str.naive-word-wrapper` (whose first line
is one column shorter than the rest). It replaces the approximate copy in
`builtins::exception_message`, so `X::Buf::AsStr` and the unexpected-adverb
messages now break at exactly rakudo's columns too.
