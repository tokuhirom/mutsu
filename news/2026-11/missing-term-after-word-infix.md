# `Missing required term after infix` for `and`, `or`, `Z`, `=>`, `=`, ...

#16986 gave `42 *` rakudo's `X::Comp::AdHoc: Missing required term after infix`,
but only for operators whose operand parser names itself in its error. Word
operators (`and or xor andthen orelse notandthen but does`), `&& || ^^`, the
meta `Z`/`X`, `...`/`...^`/`^...`, `=>`, `:=`, `ff`/`fff` and a plain `=` failed
with the generic "Confused" text. The parse-error report now also recognises
these from the source in front of the failure when nothing but a statement or
bracket end follows; a declarator's `my $x =` stays "Malformed initializer".
Still generic: `??` (rakudo is "Confused" there too), a lone prefix `!`, and the
assignment-metaop forms such as `+=`.
