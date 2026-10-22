# `is Array` subclasses accept a leading type object

`class MyA is Array {}; MyA.new(Any, 'b')` died with "Cannot convert type object to Int":
the native-constructor guard meant for numeric bases (`class I is Int`) also fired for an
`Array`/`List` subclass whose first positional was a type object. The guard now skips them,
so `Any` and other type objects are elements, as with `Array.new` (#14383).
