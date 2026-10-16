# IO::CatHandle's methods are rows of the method table

The 42 methods of `IO::CatHandle` that mutsu implements are now rows of `IO::CatHandle`, found by their owner
(ADR-11276 §9.132), instead of two name gates in the instance dispatch and the mutating method call. A read through
any receiver (a variable, an array element, a temporary) advances the one cat, because the methods write its shared
state, and `.can` finds them.
