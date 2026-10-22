# A module method call leaves the caller's object-hash key type alone

A method of a `unit class` that stores into its file-scope `my %seen{List}`
runs with that hash's key-type lane injected into its frame, and the method
return merge copied the lane back into the caller's env. The caller's own
`my %seen{Any}` then rejected a `Hash` key with "expected List"
(`PDF::COS::Array`/`Dict` against `PDF::COS::Util`, #14595). The merge now
treats a key-type lane of a class static like the value-type lane it already
dropped.
