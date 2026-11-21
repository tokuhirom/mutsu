# Core comparisons and `~` autothread on the generic operator route

A module that `use`s another module's operator candidates (Red's
`infix:<eq>` / `infix:<==>` for its AST nodes) and has a `sub EXPORT` leaves
its importer parsing `eq`, `==` and `~` as calls of the user operator. At run
time no user candidate is in scope there, so the call fell back to the plain
reduction table, which compared a Junction operand whole: `"yes" eq any(<y
yes>)` was `False`, `"a" ~ any(<b c>)` was `"aany(b, c)"`. Red's
`red migration drop-tables` and `reset` therefore cancelled even when the user
typed `yes`.

The generic infix fallback now hands `eq ne lt gt le ge`, the numeric
comparisons and `~` to the one implementation their opcodes use, which
coerces the operands and autothreads a Junction. Red 0.2.16's
`t/162-migration-reset-cli.rakutest` passes 18/18, as on rakudo (#18478).
