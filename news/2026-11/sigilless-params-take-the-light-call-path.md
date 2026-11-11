# Sigilless parameters take the light call path

A routine with a sigilless parameter (`sub absent(Mu \candidate)`,
`sub f(\x)`) used to be refused by the positional-light call path outright,
so every call bound through the general binder: about 50,000 instructions
against about 3,000 for the same routine with a `$` parameter. Modules that
spell their helpers with `Mu \obj` throughout -- AttrX::Mooish, Monad -- paid
that on every call.

A sigilless parameter is implicitly raw. It now binds on the light path with
the container transport `is rw` parameters already use (ADR-0109): a caller
variable is promoted to a shared cell that both the caller and the parameter
hold, so writes reach the caller and caller writes made during the call are
seen; a value (a literal, a call result, `self`, a type object, a read-only
caller binding) binds as a read-only term with the same read-only mark the
general binder records. Each argument is classified per call, and any shape
the light path cannot decide with certainty -- a name-aliased source, a
read-only binding cell, a multi-dimensional slice, a dynamic or twigilled
source -- still takes the general binder.

`sub sa(Mu \obj) { 1 }; sa($c)` went from 8.6 us to about 1.0 us per call
(50,300 to 8,900 instructions), and `sub g(Mu \x) { 1 }` called with a type
object from 7.1 us to 2.0 us (release build). Methods with sigilless
parameters are not covered yet: the method fast path has no caller code to
promote a variable against.
