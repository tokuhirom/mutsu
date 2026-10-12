# Longest-token ranking measures a parameterized rule through its one body

To rank alternatives such as `<value=block($indent, 0)> | <value=block-string($indent)>`,
mutsu measures each branch's declarative prefix without running the call. The
measurement ignores a call's arguments. For a rule whose parameters are bound at run
time, that meant measuring a copy of the body with `$indent` spliced in from whatever
value the environment happened to hold. The copy was also re-interpolated on every
measurement, about 2,100 times per `load-yaml` of the benchmark document.

The measurement now walks the same body every call of the rule runs. There `$indent`
is read at match time, so it ends the declarative prefix the way rakudo's interpolation
does. That body is memoized per rule. One warm `load-yaml` went from ~258M to ~252M
instructions (−2.4%; ADR-12917, #12917).
