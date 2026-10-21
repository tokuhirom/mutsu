# A `Proxy` subclass's methods use its own attributes

A class that `is Proxy` can now declare attributes and methods that work on
them, as AttrX::Mooish's `AttrProxy` does. Before this change, every method
of such a class died:

- A method reading `$!x` died with "Cannot look up attributes in a Any type
  object".
- A private call `$p.VAR!m(...)` was rejected as an unqualified private
  method call.

A Proxy subclass instance now carries its attributes in the same shared
attribute cell an ordinary instance uses, so the class's methods run with the
Proxy as `self` and that cell as their attribute store. Plain assignment, `:=`,
`⚛` and `cas` on `$!x` all work, and the instance is still a container that
FETCHes and STOREs. Private methods are callable on the instance under the
usual access rules. Attributes without an initializer start at their type
object or `is default` value, not `Nil` (#14259).
