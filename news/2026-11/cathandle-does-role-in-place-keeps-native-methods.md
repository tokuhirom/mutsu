# A cat handle with a role mixed in place keeps its methods

`$h does R` on a variable holding an `IO::CatHandle` renames the instance's class to
`IO::CatHandle+{R}` in place. The native dispatch that hands a cat handle its methods matched the
class name exactly, so every native method of the mixed-in handle died: `$h.get` gave "No native
method 'get' on 'IO::CatHandle+{R}'". The expression form (`IO::CatHandle.new(...) does R`) and an
`IO::Handle` with a role were unaffected.

The mutable native dispatch now walks the receiver's MRO to `IO::CatHandle`, as it already did
for promises, channels and supplies. The handle keeps its read state between calls. Found while
working on IO::CatHandle::AutoLines (the LN distribution); `t/io/cathandle-does-role-in-place.t`
pins it.
