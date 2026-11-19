# A metamethod on a class with a role-mixin or subclassed metaclass answers directly

`Foo.^find_method(...)`, `.^find_private_method`, `.^attributes` and the other builtin
metamethods used to be answered directly only when the class's metaobject was a plain builtin
`ClassHOW`. A class whose metaobject had a role mixed in, which is every class AttrX::Mooish
touches (`$*PACKAGE.HOW does AttrX::Mooish::ClassHOW`), or a user subclass of
`Metamodel::ClassHOW` (a custom `EXPORTHOW`), instead went through the general method-call
cascade twice: once for `.^find_method` on the type and once for `find_method` on the HOW.

The builtin-metaclass lane now also accepts those metaobjects, as long as nothing in front of
the builtin metamethod provides one: no mixed-in role declares the method (directly, through a
role it `does`, or as a public attribute or mixed-in value), and no class on the metaclass
subclass's MRO declares it. The plain `.^meta` path tries the lane too, not only the
`CallMethodMut` one.

`.^find_method` on an AttrX::Mooish class went from ~10.5 µs to ~3.5 µs per call (release
build). AttrX::Mooish calls it twice per lazy attribute build (`invoke-opt` looking up the
builder), so `t/090-concurrent*.rakutest` saves it 200,000 times over.
