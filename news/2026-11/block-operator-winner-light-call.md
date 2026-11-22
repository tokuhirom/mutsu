# A block's own operator runs its winner on the light call path

Since a block's own `multi infix:<op>` binds lexically (#18652), every call
of such an operator went through the general closure call: a full call
frame, the general signature binder, a fresh env overlay and the copy of the
candidate with its family's dispatcher bound. That cost about 50k
instructions per operator call, against about 20k when the same candidate
was dispatched by name. The winner is a routine's own compiled body, and a
keyed dispatch program has already checked every constraint of its
parameters, so it now binds through the verified positional light bind
(ADR-18580 D2), as the by-name dispatch of the same candidate does. That
covers operator families whose body reads no variable of its declaring scope
by name. The copy of the candidate is made only when the general call still
runs it. `bench-multi-dispatch`'s operator section goes from 1.93G to 0.79G
instructions.
