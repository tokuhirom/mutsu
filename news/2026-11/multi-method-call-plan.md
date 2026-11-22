# A type-deterministic multi method replays its call plan

The method-call plan (ADR-15737 D1) lets a plain method call site replay its
resolution and binding decision once the first call has made it, instead of
walking the whole dispatch tail on every call. Multi methods were never
planned, because a `where` candidate makes the winner depend on argument
values. A multi family with no such candidate (no `where`, literal, subset,
smiley or `is rw` candidate) picks its winner from the argument types alone,
which the plan's key holds, so such a family is now planned too (ADR-15737
D1b). `callsame`, `nextsame` and `samewith` from the winner keep their
deferral frame. `bench-multi-dispatch`'s multi-method section goes from 694M
to 565M instructions.
