# `.resume` after a parameter-binding failure continues the program

A call compiled to `CallTrir` never recorded a resume point, so `.resume` of the
`X::TypeCheck::Binding::Parameter` it raised ended the whole region instead of continuing after the
call. The op now records it like `CallFunc` does. Also, an exception raised by the resumed
remainder of a mainline `CATCH` body is dispatched to that same handler again (it used to escape as
uncaught). Closes #12933.
