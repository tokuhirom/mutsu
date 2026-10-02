# A CATCH over a cyclic object graph no longer overflows the stack

Template::HAML's `t/0020-grammar.rakutest` stopped after 57 of 67 assertions
with "Alarm clock". It was not a hang: mutsu overflowed its stack, and the
crash-report handler's `alarm(10)` guard then killed the wedged process.

When a `CATCH` handler runs for an exception thrown in a deeper routine,
mutsu writes the handler's by-name stores back to the installing frame, and
for each name asks whether the throw site and the installing frame see "the
same variable". That question compared the two bindings with structural
`PartialEq`, which walks container and object contents. Two same-shaped
cyclic graphs under one name (a tree whose children point back at their
parent, held by a `$node` in both frames) recursed forever.

The check now uses binding identity (`Value::same_binding`), which is O(1)
and never walks contents — what "the same variable" means in the first place.
