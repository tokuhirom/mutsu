# Unspaced hyphens no longer turn `now-$x` into a silent negative duration

`now-$x` can begin a kebab-case identifier, so it is not the same spelling as
`now - $x`. mutsu previously parsed the form as a bareword subtraction. Inside
a string interpolation block that bareword numerified to zero, silently
producing `-$x` instead of reporting a compile-time error.

The expression parser now reports the same undeclared-`now` error in ordinary
expressions and in `{ ... }` interpolation blocks. Interpolation parsing also
propagates fatal errors from its nested expression parser instead of silently
discarding the block.

The regression test pins both rejection paths and confirms that whitespace
continues to select subtraction.
