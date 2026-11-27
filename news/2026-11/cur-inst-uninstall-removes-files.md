# `mzef uninstall` really uninstalls

`CompUnit::Repository::Installation.uninstall` only deleted the
`dist/<id>.json` record, and only when the distribution carried a `dist-id`
attribute. zef uninstalls a `Zef::Distribution.new(|%($installed.meta))`,
which has the id only in its meta, so `mzef uninstall` reported success and
left the distribution installed and loadable. Uninstall now finds the entry by
dist id (attribute or meta) or by name/ver/auth/api identity, and removes the
record together with the sources, resources and scripts it owns, as rakudo
does; like rakudo's it returns `True` (#19248).
