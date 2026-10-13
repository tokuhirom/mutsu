# A nested BEGIN is lifted past a token, rule or regex of its scope

A `my token` (or `rule`, `regex`) declared ahead of a nested BEGIN kept that
BEGIN on the old in-place path, and with it every later nested BEGIN of the
unit: `sub f { my token t { x }; BEGIN say "a" }; sub g { BEGIN say "b" }`
printed neither. Such a declaration, when its pattern runs no code and reads no
variable, is now declared again in the lifted BEGIN's block, so
`BEGIN say so "axxb" ~~ /<t>/` works at compile time too (#13096).
