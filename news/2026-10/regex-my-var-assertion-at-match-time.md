# `<$n>` reads an in-regex `:my` variable when it is matched

`<$var>` interpolates a variable's value as a regex. mutsu compiled the value in
when the pattern was parsed. For a lexical declared inside the pattern there is no
value at that point:

```raku
"x a" ~~ / :my $n; { $n = "x" } <$n> /;        # was False, now True
"x a" ~~ / { 1 } :my $n; { $n = "x" } <$n> /;  # died "Variable '$n' is not declared"
```

A leading `:my` was hoisted with only its initializer, so a code block's later
assignment was missed. A mid-pattern `:my` was not visible at all.

For a name the pattern declares (an in-regex `:my`, a hoisted leading `:my`, or a
rule's runtime parameter), the parser now emits a match-time `VarAssertion` atom.
It shares the `$x` interpolation op, and when matched it reads the variable the
way rakudo's `INTERPOLATE_ASSERTION` does:

- a `Str` is parsed as a regex, under the same MONKEY-SEE-NO-EVAL policy as before;
- a `Regex` is entered;
- the callee runs as a capture-isolated, backtrackable frame;
- the enclosing `:i` applies to it.

(#13029)
