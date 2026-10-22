# `CompUnit::Handle.export-how-package`

A loaded unit's handle now answers `export-how-package`: a `Stash` with one package
per `EXPORTHOW` directive the unit declares (`DECLARE`, `SUPERSEDE`), or a type object
when it declares none. The entries are collected once per unit with the other load
facts. This is the API CompUnit::Util's `re-exporthow` calls (#14173).
