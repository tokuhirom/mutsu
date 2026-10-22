# ValueClass slice: bound attributes, container initializers, WHICH keys

- `Attribute.set_value` binds a container-less value into a scalar attribute, so a later `$!a = ...`
  dies "Cannot assign to an immutable value", as under rakudo.
- A DECLARE'd declarator's `<kw>-attr` metaclass supplies the initial container of `@`/`%`
  attributes through `container_initializer`.
- While a HOW's `compose` hook runs, `.^find_method` sees only the class's own methods, so a hook
  that adds a `WHICH` unless one exists adds it.
- A Set/Bag subscript keys an object by its user `.WHICH` (`%set{Obj.new}`).

ValueClass's `t/01-basic.rakutest` goes from 25 to 37 passing assertions (#14588).
