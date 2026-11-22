# `X::Undeclared` with no suggestion adds the forgotten-`sub` hint

When there is no suggestion, rakudo's `X::Undeclared.message` adds
"Perhaps you forgot a 'sub' if this was intended to be part of a signature?"
and word-wraps the result at 72 columns. mutsu's formatter only rendered the
suggestion branch, and an undeclared variable inside `EVAL` built its own text
ending in a bare `.` (#18731).

Both now use one value-layer helper,
`value::exception_message::undeclared_message`.
