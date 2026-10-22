# `.resume` of a refused typed element store continues in place

`@a[0] = "x"` on `my Int @a`, `%h<a> = "z"` on `my Int %h` and a `my Int @c = 1, "y"`
declaration now resume at the raise site like the scalar stores do (#14153). A
single-element store leaves the constraint's type object in a slot that did not
exist (an existing slot keeps its value); an `@` declaration replaces each refused
element with the type object and offers the next one to the handlers in turn.
