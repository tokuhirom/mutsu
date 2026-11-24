# `.?` answers `Nil` on a user subclass of Array or Hash

A method miss on an instance of `class B is Array` names `Array`, the type
whose dispatch missed. The `.?` own-miss check (#19113, #19138) compared that
name only with the core type hierarchy. So `self.?cb-init` in a `PDF::COS::Array`
re-threw `X::Method::NotFound` where rakudo returns `Nil`. This broke most of
PDF's test files, among them the `index(TestArray:U:)` failure in
`t/cos-tie-index.t`.

The VM's optional-call sites now also ask the interpreter whether the receiver
is of the named type, so a user class's ancestry counts. A miss inside a found
method on another receiver still propagates.
