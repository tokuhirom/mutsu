# A symbolic lookup no longer finds its caller's `my class`

`sub g { ::('SecretType') }` called from `sub h { my class SecretType { }; g() }`
returned `h`'s lexical class. A `my class` is lexical to the block that
declares it, so in Raku the lookup fails there. A routine whose frame has a
static link (ADR-12529 phase 3) now skips its callers for a bare type-shaped
name as it does for a lexical variable; a package-scoped class the caller
declared is still found through the symbol table.
