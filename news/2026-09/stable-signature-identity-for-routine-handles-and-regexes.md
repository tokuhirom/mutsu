# Routine handles and regex values retain their materialized `Signature`

Repeated `.signature` reads on a name-based `Routine` handle such as `&say`,
or on a `Regex` value, used to create a fresh `Signature` each time. They now
return the same object, as they do in Rakudo.

Regex cache keys retain the underlying payload `Arc`, so two separate,
textually identical declarations remain distinct and a released allocation's
address cannot be reused for a later declaration under `MUTSU_GC=on`.

Pinned by `t/regex/regex-routine-introspection.t`. Closes [#8417](https://github.com/tokuhirom/mutsu/issues/8417).
