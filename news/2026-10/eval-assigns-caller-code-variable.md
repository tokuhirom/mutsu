# `EVAL` can assign a caller's `my &h`, and `::('EXPORT::…')` finds the unit's exports

`my &h; EVAL q[&h = { 1 }]; say h()` now prints `1`, as in rakudo. The `EVAL`'d
unit compiled `&h = ...` as the read-only store a routine name gets ("Cannot
modify an immutable value"), because it did not know the caller's static
scopes declare a `my &h`; and once that was fixed, the EVAL's exit rolled the
caller's code variables back to their pre-EVAL values (#13657). The EVAL now
compiles against the code variables the scopes around its call declare, and
keeps the ones it assigned.

`::('EXPORT::DEFAULT::&mine')` in a program resolves the program's own
`is export` routine: a leading `EXPORT` is the running unit's own export
package (#13808).
