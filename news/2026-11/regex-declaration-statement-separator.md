# A `token`/`rule`/`regex` declaration needs a separator before the next statement

`grammar G { token a { x } token b { y } }` puts two declarations on one line
with no `;` between them, and rakudo rejects it with "Strange text after
block". mutsu already applied that check to `sub` and `method` declarations,
but not to the regex declarators, so it accepted the line silently (#19030,
found while writing a grammar test for #17216). The statement-boundary check
now treats a `token`, `rule` or `regex` declaration as a block-ending
statement, just like `sub` and `method`.
