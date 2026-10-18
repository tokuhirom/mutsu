# An exported method called as a sub forwards its named arguments and `$?CLASS` default

`method selector-to-xpath($class = $?CLASS: Str:D :$css!, ...) is export` (CSS::Selector::To::XPath)
imports as a sub whose forwarding body dropped every named parameter and evaluated `$?CLASS` with no
class in scope (`Nil`). The synthesized sub now defaults to the declaring class and forwards the
method's named parameters as one slurpy hash, so the method rebinds and checks them.

Also: `method m(@l is copy)` given an itemized array (`$[1]`) copied the Scalar wrapper in the method
fast path, so `@l.map` iterated the array as a single item after a `shift`; the method path now
deitemizes like the sub binder.
