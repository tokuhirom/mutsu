# Collation, Attribute and Variable have dispatch shapes

`Collation`, `Attribute` and `Variable` instances are closed dispatch shapes, so
every entry (VM native, call-site lane, by-name, mutating) reaches their rows
through the method table. The per-site `collation_owners_of`,
`attribute_owners_of` and `variable_owners_of` hooks and their `OWNER_ONLY`
flags are gone. `Stash` keeps its owner hook: a shape would hide its
associative indexing from the hash paths.
