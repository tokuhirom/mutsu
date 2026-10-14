# A `Method` object primed with `.assuming($invocant)` binds `self`

`$type.^lookup('m').assuming($obj)` returned a copy of the method's callable
with `$obj` as an assumed argument, and calling it bound that argument to the
callable's anonymous invocant parameter without ever setting `self`, so the
first `self` in the body died with "'self' used where no object is
available". The primed callable now keeps the method's signature (minus the
assumed arguments) for introspection but runs through the `Method` object's own
`CALL-ME`, which dispatches the owner's candidate on the invocant -- the same
path a direct call takes. A multi method's dispatcher object is primed the same
way.

A direct call with a *type object* as the invocant
(`C.^lookup('m')(C, 1)`) took the callable path too and had the same problem;
it is now dispatched on the type object like an instance.

Math::DistanceFunctions' `get-distance-function`, which returns
`self.^lookup($name).assuming(self)` from a role method, depended on both:
its `t/01-distance-functionish.rakutest` and `t/02-distance-functions.rakutest`
now produce the same TAP as under rakudo.
