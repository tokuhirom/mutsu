# A class-scoped accessor shadows the built-in method of its name

`class C { my @.list of F }` declares a `list` accessor, which should hide
`Any.list`. It did not: `C.list` answered the built-in `(C,)`, so
`C.list.push(...)` died with "Cannot call 'push' on an immutable 'List'". A
`my $.elems` accessor even recursed until the stack overflowed. Both VM method
paths and the interpreter's by-name dispatch now let a class-scoped accessor
win, on the type object and on an instance. Assigning through the accessor
(`C.list = ()`) also keeps the declared `Array[F]` container, instead of
storing the assigned `List`.

BDD::Behave records failures with `Failures.list.push(...)`, and this
behaviour is what made its suite report false failure counts (#13131).
