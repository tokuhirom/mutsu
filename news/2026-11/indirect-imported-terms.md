# Indirect lookup finds terms imported by `sub EXPORT`

`::('name')` now checks the active lexical import hooks for a sigilless term.
This lets it find plain and Proxy values exported by a module, including
lowercase names, while keeping block-scoped imports within their lexical scope.
Env's `t/01-all.rakutest` exercises this when environment keys are lowercase.
