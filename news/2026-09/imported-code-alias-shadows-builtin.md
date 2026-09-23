# Imported CODE aliases shadow same-named builtins

Bare calls now honor a callable alias installed by a custom `EXPORT` hook
before dispatching a same-named CORE builtin. Ordinary exported subroutines
and custom code-valued exports therefore have the same shadowing behavior.
