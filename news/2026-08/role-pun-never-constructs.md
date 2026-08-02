# Role method punning no longer constructs an instance

Calling a method on a non-parameterized role type object now always puns the
role to a class and dispatches on that class's type object. Previously, roles
without their own `new` method were silently constructed, so instance attribute
defaults could make an invalid type-object access appear to work:

```raku
role C { has $.x = 5; method get { $!x } }
C.get; # now reports that C is a type object
```

The type-object path also handles roles composed from other callable roles.
Punning no longer duplicates inherited multi-method candidates, and a punned
type object now satisfies the composed role constraint on an inherited
`::?ROLE:U` invocant. This restores all 39 tests in
`roast/S13-overloading/typecasting-long.t` regardless of whether the parent
role was punned first.

Explicit `.new` calls on roles still construct instances normally.
