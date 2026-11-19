# TRIR runs routines with a built-in nominal return type

A statically typed routine with a declared return type such as `--> Str` or `--> Bool:D` used to
decline TRIR (ADR-0110), because Stage 1 admitted only `--> Nil` and a definite constant. Such a
routine took the light call path even when nothing else in its body stood in the way. Modern
module code annotates most routines this way.

TRIR now admits a return type that the light call path's return check serves by itself, the same
`is_fast_return_type_name` test that path applies. The chunk carries the check's precomputed plan
(`CompiledFunction::return_fast_plan`). `run_trir_routine` runs the light path's check, now the
shared `light_check_return_type`, on the routine's value. So Nil, a Failure and a matching type
object pass, and a mismatch raises the same `X::TypeCheck::Return`. A user class, a subset or a
coercion return type still declines.

`my sub plain(Str $ch --> Str) { $ch }` called in a loop went from about 7.7k to 4.3k instructions
per call (callgrind, release build). It is now as fast as the same sub without a return type.
