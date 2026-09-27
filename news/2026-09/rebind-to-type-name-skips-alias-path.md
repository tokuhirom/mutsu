# A `:=` rebind to a type name no longer runs the named-alias path

A statement-level rebind whose source is a bareword type, package or term
name (`$s := IB`) cost almost three times as much as a rebind to a literal
(`$s := 1`). The compiler routed the source through `compile_call_arg`, which
tags every bareword argument with `WrapVarRef` (a shape tag meant for `is rw`
call-argument matching). `SetLocal` then saw a named bind source called `IB`
and ran the whole variable-alias machinery on every execution: it wrote a
`__mutsu_sigilless_alias::s` key, probed readonly state, walked every call
frame's saved env for a variable named `IB`, and mirrored the bind into the
env. None of that means anything for a name that is not a variable.

The rebind now compiles a bareword that the compiler knows is not a variable
(not a local slot, a sigilless binding, or a constant) as a plain value, the
same way a literal source is compiled. The scalar bind still marks the target
immutable, so `$s := IB; $s = 1` still dies. A bareword that names a sigilless
variable (`my \y := $v; $s := y`) keeps its alias and still writes through.

Callgrind instructions per loop iteration (`while $i < $n { <stmt>; $i = $i + 1 }`,
difference between 10 and 5,010 iterations, profiling build):

| stmt | before | after |
| --- | ---: | ---: |
| `$s := IB` | 17,861 | 9,016 |
| `$s = IB` | 6,862 | 6,862 |
| `$s := 1` | 6,924 | 6,925 |

The ~2.1K instructions still between `$s := IB` and `$s := 1` are the
run-time type-name resolution in `GetBareWord`, which `$s = IB` pays too
(#9651 stays open for that part).
