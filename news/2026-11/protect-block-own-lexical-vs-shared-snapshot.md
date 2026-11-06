# A `Lock.protect` block reads the enclosing frame's own lexical, not a stale shared-variable snapshot

Once a program has started a thread, scalar stores publish a by-name snapshot to the shared
variable store. `Lock.protect: { ... }` pulled that snapshot over the block's free reads of
a name even when the enclosing routine owned a lexical of that name, so a method's
`$name` parameter could read another scope's `$name`. Red's `MetamodelX::Red::Model.alias`
hit this and joined the `author` relationship under the alias `post` instead of
`post_author`, so RedFactory's `.create` died with `ambiguous column name` (#16881). The
protect fast path now reads such names from the enclosing frame's slot, as it already did
for the block's own captured locals.
