# Subrule calls with `$*` parameters or object arguments run on the compiled regex engine

A `<subrule>` call whose callee declares a `$*` parameter, or that passes an object or closure
argument, used to hand the whole callee to the regex tree walk. Worse, as soon as any rule in a
program declared a `$*` parameter, *every* subrule call of every grammar bridged to the walk.

Such a call now runs as an ordinary frame of the compiled engine (ADR-0135 Slice E, eighth part).
The callee's binding window is installed when the call is made, removed when the callee returns or
fails, and installed again when backtracking resumes inside the callee. Each install and uninstall
is an undoable entry on the run's register trail, the same mechanism that closure scopes of spliced
Regex values use. The callee's action still sees its `$*` parameters, because the return records
them on the callee's Match.

`MUTSU_VM_STATS`'s `regex-walk:` line no longer reports `args-opaque` or `dynamic-param` bridges.
