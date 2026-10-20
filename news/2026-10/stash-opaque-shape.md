# `Stash` is an opaque dispatch shape

`Stash`'s rows (`merge-symbols`) were reached through an owner hook in the
instance dispatch because a shape would hide its associative indexing from the
hash paths. Opaque shapes are visible to the table's owner lookup and invisible
to `dispatch_shape()`, so `Stash` is `DispatchShape::Stash` now and
`stash_owners_of` and its hook are gone.
