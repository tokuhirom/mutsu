# Config::Parser::NetRC remains blocked on a shared trait re-export gap

The random ecosystem run re-measured `Config::Parser::NetRC` 0.0.1. Rakudo
passes its only baseline file, `t/01-basic.rakutest`, with 2/2 assertions, but
mutsu still dies before the plan with `Can't use unknown trait 'is' ->
'json-skip-null'`.

The failure reduces to `JSON::Class` re-exporting `trait_mod:<is>` from
`JSON::Marshal`; mutsu cannot currently make that re-export visible to an
importer. This is the shared deep gap tracked by [#8121](https://github.com/tokuhirom/mutsu/issues/8121),
so no distribution-specific workaround was added.
