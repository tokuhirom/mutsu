# A role body now runs before its methods are composed, with the class parents visible

Composing a role into a class used to push the role's methods into the class before the role body ran, and the body saw a class shell without its parents. So `::?CLASS.^find_method('greet')` in a role body returned the role's own `greet` (infinite recursion for the wrapping idiom WriteOnceHash uses), and `::?CLASS.^mro` lacked the parents.

The body now runs against a stand-in class that carries the class's parents (roles excluded, as in Rakudo's `.^mro`), and the role's methods are pushed afterwards, followed by the nested-method-capture reapplication. `t/oo/role/role-body-sees-class-parents.t` pins it (#11628).
