# Object increment and decrement require step methods

Incrementing an object without `succ`, or decrementing one without `pred`,
now propagates `X::Method::NotFound`. Prefix and postfix forms leave the
variable unchanged when the step fails. An exception raised inside a user
step method also propagates, including a missing method with the same name.
Native instance methods such as Date's successor and predecessor keep
their existing dispatch.
