# An unsupplied optional `&` role parameter is the Callable type object

`role R[&rel1, &rel2?]` composed as `$x but R[&f]` read `&rel2` as `Nil`, not as
the `Callable` type object a sub's `&cb?` is, so passing it on
(`R[&rel1, &rel2, ...]`, as `Red::Attr::Relationship.transfer` does) found "No
matching candidate found for the parametric role". The candidate resolution now
reports the `Callable` type object for such a parameter, and the mixin method
dispatch installs it as the `&rel2` callable binding rather than a bare `rel2`
scalar (#14094).
