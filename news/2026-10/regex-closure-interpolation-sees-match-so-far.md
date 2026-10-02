# Regex `<{ code }>` sees the match so far in `$/`

The code of a `<{ … }>` closure interpolation used to run with an empty `$/`, while a `{ … }`
block and a `<?{ … }>` assertion saw the match so far. `eval_regex_closure_interpolation` now binds
`$/` to a Match object covering the text matched from the match start up to the atom, as the
inline-code path does, so `"abc" ~~ / a <{ say ~$/; 'b' }> c /` prints `a` like Rakudo. Pinned by
`t/regex/match/closure-interpolation-match-so-far.t`. Closes #10418.
