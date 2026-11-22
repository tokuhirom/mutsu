# `use Test` lifts the regex-interpolation check; `.match(:as(Str))`; angle-brackets.t whitelisted

In rakudo the `MONKEY-SEE-NO-EVAL` pragma is the lexical symbol
`&MONKEY-SEE-NO-EVAL`. When rakudo compiles a string interpolated into a regex,
it calls that routine and goes by its answer. rakudo's `Test` exports
`sub MONKEY-SEE-NO-EVAL() is export { 1 }`, so any file that does `use Test` may
interpolate a string that holds a code block. mutsu only knew the
`use MONKEY-SEE-NO-EVAL` spelling. Now an import that binds
`&MONKEY-SEE-NO-EVAL` calls the routine once and sets the pragma from its
answer. This works for a block-scoped `use Test` too, which binds the routine
lexically, and an inner `no MONKEY-SEE-NO-EVAL` still overrides it (#18740).
The same routine declared in the file itself is #18748.

`Str.match(Regex, :as(Str))` returns the matched text instead of the `Match`.
The `:g`, `:ov`, `:ex`, `:x` and `:nth` forms honour it too, while `:c`/`:p`
and any type other than `Str` ignore it, as rakudo's dispatch does.

With both fixes, `roast/S05-metasyntax/angle-brackets.t` passes all 95 tests
and is whitelisted. `TODO_roast/BLOCKERS.md` had listed it as "no oracle"
because raku had been run on the unfudged file. Run through roast's fudger,
rakudo passes it 93/95, and the pseudo-package files 156/159 and 158/161
(#18742).
