# A method call no longer allocates a `String` for its samewith context's name

Every method call pushes a samewith context so a deferral builtin (`callsame`,
`nextsame`, ...) or a parameter-concreteness error can find the enclosing
routine's name. The context stored the name as a `String`, so each call
allocated and freed one even in a program that names no deferral builtin
anywhere, where the only reader left is the error message. The field is now a
`Cow<'static, str>`, and the method-dispatch call sites that already hold the
method's interned `Symbol` (`push_method_dispatch_frame_sym`) borrow the
symbol's `&'static str`. Callers that only have a `&str` keep the owned copy.

On a no-parameter method called 100,000 times, allocations went from 2 to 1 per
call and instructions from 1,162.3M to 1,147.9M, about 130 per call (second
callgrind run of each, release build, #15874's binary against this branch).
Pinned by `t/exceptions/method-name-in-concreteness-error.t`: the method named
in the error is the called one, also in a nested call.
