# `UInt` parameters and return types take the light call path

The light call paths check a parameter's type inline only for a short list of
types: `Int`, `Str`, `Num`, `Bool`, `Rat`, the native types, `Any` and `Mu`.
Any other constraint sent the call through the general binder. That binder
pushes a full call frame and resolves the return type's name on every call.
`UInt` was not on the list, so FiniteField's
`multi infix:<*>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }` paid
the general path on every arithmetic operator.

`UInt` is now a light-path type (`FastParamType::UInt`). For a plain `Int`, a
big integer or a `Bool`, the check is a sign test. Every other value goes to
the same type matcher the general binder uses: a type object, an allomorph, an
enum value or an `Int` subclass. So a light call accepts and rejects exactly
the same arguments as before. A rejected negative `Int` keeps rakudo's wording,
"Constraint type check failed in binding to parameter '$x'; expected UInt but
got Int (-1)", and a `--> UInt` return still lets `Nil` and a `Failure`
through.

On #10110's repro (the operator loop timed with `now`), callgrind went from
1.421G to 0.756G instructions (-47%). The release build, median of 7 runs on a
4-core container, takes 0.097-0.104 s against rakudo's 0.096-0.101 s for the
same loop. That meets #10110's close condition of 1.2x rakudo.
