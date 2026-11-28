# A method with a user-class parameter runs in the lean frame

A method whose positional parameter is typed with a user class
(`method distance-to(Point $other)`) used to be refused by the lean method
frame, so every call took the general fast binder, which resolved the type
name again on every call: a type-capture probe, a type-alias probe, a core
catalog lookup, string-keyed hash probes and a scope lookup of the name, about
4.3k instructions per call (#19293).

The lean frame now admits a plain user class (a registered class that is not a
role, subset or enum, and that nothing in scope renames when the call is
planned). Per call, an argument that is an instance of exactly that class
passes. Anything else -- a subclass instance, the type object, a junction, a
value of another type -- is handed to the general binder, which decides it in
full and raises the same error as before.

A `method typed(P $o) { $!x - $o.x }` call went from 20,939 to 15,480
instructions (callgrind slope, release build; the untyped form is 16,578).
`benchmarks/method-call.raku` went from 469.7M to 405.2M instructions (-13.7%)
and from 63.8 ms to 49.0 ms wall against rakupp v5.9.0's 34.9 ms.
