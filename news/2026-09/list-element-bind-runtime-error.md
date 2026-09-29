# Report invalid subscript binds at run time

Binding to an immutable subscript target now raises `X::Bind` during execution,
after preceding statements and operand side effects have run. The exception
includes the source location, matching Rakudo's behavior for list, numeric,
and string targets.
