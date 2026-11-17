# An attribute name is no longer looked up as a package variable

`SetGlobal` mirrors every global write into the `our` store, an attribute
container's included, so `@!inlines` sits in the unqualified-name index that
gates the `our`-container redirect. Every by-name read of the attribute in a
method body -- the snapshot and write-back around `@!inlines.List`, for one --
therefore walked four candidate packages' `::` chains looking for a
package-qualified `@!inlines`, a key no package can have: the candidate builder
rejects twigil names for every package.

That name-only half of the candidate check is now its own helper,
`name_has_package_candidates`, asked once before the walk in
`our_package_var_key` and in `package_chain_var_fallback`. The answers are
unchanged; only the walk that could not succeed is skipped.

Callgrind, release build: constructing an object whose `TWEAK` does
`@!inlines := @!inlines.List` went from 146.9k to 133.6k instructions (-9%);
a Markdown::Lex pipe-table row from 1.608M to 1.573M. Refs #16874.
