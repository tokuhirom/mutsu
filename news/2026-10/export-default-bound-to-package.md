# `OUR::<DEFAULT> := Pkg` in `my package EXPORT` exports the whole package

annotations 0.1.0 publishes `annotations::containers` with
`my package EXPORT { OUR::<DEFAULT> := annotations::containers }`, so the
module's `our` symbols (`Binder`, `Buffer`, ...) become its default exports.
mutsu bound the stash entry but exported nothing. Binding a package to an
export tag now re-exports every symbol of that package's stash under the tag,
as if each had been bound into `EXPORT::<tag>` one by one.
