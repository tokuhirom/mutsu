# `Attribute.build` runs its default in the declaring unit

`has Str $.id = ~UUID.new` handed out through `$attr.build` ran in the compilation unit of
whoever called it, so calling it from a module that never imported `UUID` died with
`Undeclared name: UUID`. The thunk now records the file it was declared in and enters that
unit while it runs, like any other closure. Found with Red's `t/082-emakei.rakutest`, whose
`MetamodelX::Red::Dirtable` builds attribute defaults this way; the test now gets past model
construction and creates rows.
