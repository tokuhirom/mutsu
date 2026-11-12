# `.^m` on a type with a user metaclass runs the metaclass method directly

`$obj.^m` is `$obj.HOW.m($obj)`. For a type whose metaobject is an instance of
a user metaclass (an EXPORTHOW declarator such as Red's `model`, or
`Metamodel::ClassHOW.new_type`) that declares `m`, the call went through the
whole general method-call probe chain before reaching that method — about
23,000 instructions of overhead per call. A lane at the top of the
`CallMethod`/`CallMethodMut` handlers now runs the metaclass's compiled
method directly with the receiver as its argument, when the call is an
unquoted `.^m` without arguments, the type declares no `method ^m` of its own,
and no wrap or `^find_method` hook is installed. Red's `Red::Column.gist` calls
`$model.^as` for every column it renders.
