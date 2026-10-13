# An exception thrown inside `COERCE` propagates as itself

`sub k(C() $x)` with `method COERCE($v) { die "boom" }` reported
`X::Coerce::Impossible` (class target) or a parameter-binding failure, and the enclosing call
(`throws-like`) was re-run because of the wrapping. Now only "no such `COERCE` / no matching
candidate" falls through to the impossible-coercion path; anything the method body throws propagates
unchanged for both class and role targets, and the parameter binder no longer wraps it. Closes #13077.
