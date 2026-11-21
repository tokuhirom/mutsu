# Assigning a rebindable `@a` from a routine fills the same array

`@a = ()` inside a sub fills the array `@a` already is, so a parameter bound to that array sees the
change. mutsu did so except when some routine anywhere `:=`-rebinds `@a` (`sub r { @a := [7] }`,
even one never called): that puts the variable's container behind a binding cell, and the
identity-preserving store compared the binding cell's content -- another cell, not an array --
with the new array, found no match, and stored a fresh array instead. A parameter holding the old
array then still saw the old elements (#17585). The store now looks through the binding chain to
the container it fills, for `@` and `%` variables alike.
