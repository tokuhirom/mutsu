# A type-only parameter may be followed by `;` or `;;`

`multi f(LOG;; $x)` and `sub f(Int;; $x)` failed to parse: the type-only parameter path accepted
`)`, `,`, `]`, `{` and `-->` after the type but not the `;` / `;;` multi-invocant separators. It now
does, matching Rakudo.
