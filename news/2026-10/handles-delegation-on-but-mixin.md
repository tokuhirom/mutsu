# `handles` delegation answers on an instance mixed into with `but`

`$holder but role { ... }` wrapped the instance, and a method the class
delegated with `has Chain $.chain handles <limit>` then died with "No such
method 'limit' for invocant of type 'Holder'": the delegation forwarder was
handed the mixin's (empty) attribute map and found no `$!chain`. It now reads the
delegate attribute off the inner instance. Red's `create-map` returns its
`ResultSeq` `but role CMModel[...]` and then calls the handled `.limit` on it
(#14094).
