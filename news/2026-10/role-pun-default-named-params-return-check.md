# A role pun with named-parameter defaults satisfies its own type checks

The default curry of `role R[::T = Any, :$flag = 1]` is registered as a class
named after its arguments, named ones included. A type check dropped the named
arguments from the constraint (#15504) but not from the registered class, so
`method m(::?CLASS:U: ... --> R) { self.new }` called on the role itself
rejected its own instance (Tree::Binary, #15769). A constraint or type object
that names a registered pun class is now taken as it is.
