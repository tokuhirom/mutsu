# Method objects keep their identity; `$*CU` resolves a method's invocant type

The static distribution's analyzer collects the unit's own methods by
`nqp::objectid` of each `RakuAST::Method`'s `compile-time-value`, and later
recognizes a call's target by looking the method up again with
`.^find_method`. mutsu built a fresh `Method` instance whenever the
metaclass lookup cache had been cleared, and any registration clears it, so
the second lookup answered a different object and static reported the
unit's methods as "external routine has no return contract". A declared
method's object now carries a stable instance id per owning class, kept in
the method's routine cell, so `.^find_method`, `.^lookup` and `.^methods`
answer the same object for the life of the class, as in rakudo. A role's
method composed into two classes is still two methods.

The implicit invocant parameter a method's signature shows in `$*CU` is
synthesized when the tree is visited, after the resolver ran, so its type
was never resolved. The resolver now records the invocant's spelling on the
method (the package's full name, or `$?CLASS` in a role) and the
synthesized `Type::Simple` carries the resolution rakudo gives it: a
`Declaration::LexicalPackage` for a plain package name, the package's
`VarDeclaration::Implicit::Constant` for a qualified one, and a
`Type::Capture` in a role, whose `lexical-name` is now `$?CLASS` rather than
its `Name` node.

All fifteen static test files now match rakudo (#18107).
