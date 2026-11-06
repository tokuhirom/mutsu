# A trailing comma after a no-paren builtin call is part of its argument list

`lc "A",;`, `flip "ab", ;` and `(sprintf "%s", "ab",)` used to leave the trailing
comma behind, so the enclosing parser wrapped the call result in a one-element
List. The no-paren listop loop now consumes a trailing comma before `;`, `}`,
`)` or end of input, as rakudo does (#16886).
