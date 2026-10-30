# A declared `multi infix:<eq>` keeps the core operator's precedence

`multi infix:<eq> (T \a, T \b)` adds a candidate to the core `eq`, which keeps its level, but mutsu took
the declared word at the trait-less additive level: `2 + 1 eq 4 - 1` read as `2 + (1 eq 4) - 1`. The
comparison words (`eq ne lt gt le ge eqv after before`) now parse at the comparison level when declared
without a precedence trait. `unicmp` and `coll` also parse at their own level, and the `unicmp`, `coll`
and `o` opcodes now try the declared candidates before their core body, so the new level does not lose
the candidate they used to reach through the custom-word path. Chaining of a declared comparison word
(`a eq b eq c`) is still left-associative; it is tracked separately.
