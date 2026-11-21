# A scope's own package wins over a hidden module name of the same spelling

Red 0.2.16's migration compiler EVALs `my package Configuration { }; model
Configuration::CliReset ...` from inside `Red::Migration::Data`, while the
`Configuration` distribution is loaded by another of Red's modules. mutsu
treated `Configuration` as that module's name, hidden from the running
compunit, and died with "Could not find symbol 'Configuration::CliReset'"
(`Configuration` alone was "Undeclared name").

A package the running scope declared itself — a lexical `my package`, a
`my class`, or a package nested in the running package — is now what the bare
name and the `Name::Member` qualifier mean there, both in the bare-word lookup
and in the qualified-name visibility gate. Red's `t/150-synchronous-cli` now
passes 17/17 as on rakudo, and `t/162-migration-reset-cli` gets past
`migration prepare` (#18409). The other failures those files showed are
separate bugs, filed as #18478 (a junction no longer autothreads `eq` after a
module with a `sub EXPORT`) and #18479 (`MAIN` with `:named-anywhere` parses
the words after `--`).
