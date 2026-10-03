# Router::Right passes: MONKEY-SEE-NO-EVAL, attributes in list literals, `A // $x = v`

Router::Right's three test files now pass under mutsu (two died before).

- `use MONKEY-SEE-NO-EVAL` (or `use MONKEY`) now allows a string carrying a
  code block to be interpolated into a regex assertion (`<$re>`,
  `<{$code}>`); before, it was always refused with `X::SecurityPolicy`.
  - The pragma is lexical: `no MONKEY-SEE-NO-EVAL` turns it off for its
    block.
  - At a module's top level it covers the module's own routines when another
    compunit calls them.
  - A module's pragma does not leak into the importing scope.
- An attribute written in a list literal (`($!name, $x)`) is the invocant's
  own attribute container. The method frame's copy was boxed into a private
  cell and published under `!name`, so a nested call of the same class's
  method on another object (`$!parent.add(...)`) read the caller's `$!name`
  as its own.
- `A // $x = v`, `(A || $x) = v` and `(A && $x) = v` assign to the operand
  the operator picked, since `//`, `||` and `&&` yield its container. This
  covers `try { ... } // $failed = True`.
