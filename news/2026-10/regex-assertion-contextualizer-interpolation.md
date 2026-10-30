# `<@(code)>` / `<$(code)>` assertions, and their aliases, in a regex

`<thing=@($*ctx.items)>` (MUGS::Games' adventure CommandParser) was rejected at load
("rule declaration without a source tree") and, once parsed, failed to match. The
assertion forms `<$(code)>` and `<@(code)>` now have a regex-tree node
(`ContextualizedInterpolation`, `Assertion::InterpolatedVar` over a `Contextualizer`
in RakuAST) and match by evaluating `code` where the cursor reaches the atom, reading a
string result as regex source as `<$x>` / `<@x>` do (the bare `$(code)` / `@(code)` atoms
keep matching literally). `<name=@array>`, `<name=@(code)>` and `<name=$(code)>` name the
interpolation's own match, like rakudo.
