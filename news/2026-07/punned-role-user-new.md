# Punned roles run their own `new`

Calling `.new` on a bare role now dispatches to a `new` declared by that role.
The temporary class used to execute the method is removed after dispatch, so it
does not shadow the role during later coercion or composition. A returned
`self.bless(...)` instance is wrapped back into the normal punned-role
representation.

The fallback constructor no longer maps positional arguments onto attributes by
declaration order. Like an ordinary class default constructor, it accepts named
attribute arguments and raises `X::Constructor::Positional` for positionals.

Pinned by `t/punned-role-user-new.t`, including recursive `self.new(:named)`
fallback and `self.bless`.
