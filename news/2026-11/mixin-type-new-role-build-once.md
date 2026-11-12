Composing a role into a class type object no longer runs the role's `BUILD` or
`TWEAK` submethod before an instance exists. Constructing the mixed type runs
each submethod once, matching Rakudo for `(Class but Role).new` and grouped role
applications.
