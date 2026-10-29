# `.resume` of an exception a native op raised in a callee frame resumes the callee

A `CATCH { .resume }` resumed an exception raised by a native op in the same
frame, but when the op raised inside a called routine (a sub, a closure, a
`Proxy`'s `STORE`) the whole enclosing block ended after the handler instead
of continuing the callee: the handler ran after the callee's frames were gone
and only the caller's own resume point existed (P5substr `t/01-basic.t`,
`$str.substr-rw(7, 1) = 'x'` out of range).

A call op that raises now offers the exception to the resume-capable handlers
at the raise site, like `die` does (ADR-0072): `.resume` continues the callee
with the call yielding `Any`, and a handler that does not resume unwinds as
before.
