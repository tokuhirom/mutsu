# A `::T` type capture is no longer read as a declared type named T

#15076 made `has_type` dereference whatever the lexical env held under a name, so the `::T` capture of a
generic routine (bound in the env as a container of the type it captured) counted as a declared type named
`T`; `T.^add_multi_method` then looked up a class literally called `T` and every Red model died with
"Unknown class for add_multi_method: T". The container is now read through only for the imported term and
package alias (the `use NativeCall` `CArray` case #15076 fixed), not for a plain lexical.
