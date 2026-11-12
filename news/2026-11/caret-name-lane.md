# `.^name` on a user class skips the general method-call probes

`$obj.^name` and `Type.^name` on an ordinary user class went through the whole
`CallMethod`/`CallMethodMut` probe chain — `shapeless_row` name scans,
`introspectable_name_listed`, the class-level attribute and MRO probes —
before `dispatch_method_by_name_1` reached its `^name` arm: about 64,000
instructions per call, against rakudo's 0.23 µs.

A lane at the top of both opcode handlers now answers it directly when the
receiver is an instance or type object of a registered user class whose
metaobject is the default `ClassHOW` (no EXPORTHOW / `new_type` metaclass)
and on whose MRO no class declares `method ^name`. Every other receiver —
a mixin, a custom HOW, a metaclass instance, a quoted or argument-carrying
call — keeps the general path. One `.^name` now costs ~4,800 instructions
including the loop around it (a 13x reduction). Red's `Red::AST.WHICH`, which
builds every hash key and optimizer comparison from `self.^name`, is the
workload that showed it (#17552).
