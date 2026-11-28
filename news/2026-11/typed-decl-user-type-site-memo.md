# A typed declaration of a user class or role memoizes its resolution per site

`my Tag $t = ...` inside a method resolved its constraint on every execution,
three times over: `SetVarTypeScoped` ran the type-capture, package-alias,
constant-alias and lexical-type probes and the package-chain qualification;
the declaration's `TypeCheck` re-ran most of them before deciding that a Nil
needs no check; and the store of that Nil ran
`nominal_type_object_name_for_constraint` to find the type object to leave in
the variable, copying the spelling and re-interning the result.

For a spelling that names a class or role registered under exactly that name,
those answers depend only on the registry, the running package and a few env
bindings. The chunk now remembers, per constraint constant, the (registry
write generation, package) under which the spelling was found to resolve to
itself (`TypeDeclSiteCaches`' user-type words); the env bindings that could
rename it -- a `::T` capture, a module package alias, a lexical type in scope,
a `Package` bound under the name -- and module visibility are asked again on
every hit. A hit registers the constant as written, seeds the type object of
that name, and lets the declaration's `TypeCheck` pass a Nil (or match a value)
without the general path. A Nil stored into any typed scalar also gets its
type object without the owned copy (`nominal_type_object_sym_for_constraint`).

Per call of a method with `my Tag $t = $x` (callgrind, profiling build):
27,887 -> 20,948 instructions; `my Str $t = Nil` 23,286 -> 19,872; a module
method with `my Tag $t = $x; my Str $a = $x` given Nil 47,151 -> 35,588. A warm
YAMLish `load-yaml` of the 60-row bench document: 104.88M -> 103.46M. Refs
#12917.
