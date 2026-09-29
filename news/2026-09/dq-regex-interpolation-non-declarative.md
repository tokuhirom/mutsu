# Runtime interpolation inside a double-quoted regex literal ends the LTM declarative prefix

`"$x"`, `"{$x}"` and `"${x}"` inside a regex literal are now treated as runtime
interpolations for alternation LTM ranking (ADR-0022 Slice 5), so
`"abc" ~~ / "$x" | a /` with `$x = "ab"` matches `a` like Rakudo instead of `ab`.
The `"..."` tokenizer arm now honours `NON_DECLARATIVE_INTERP_MARK`, the qq-thunk
splice wraps its result in the mark, and bodies made only of compile-time
`constant`s stay declarative. Closes #9909.
