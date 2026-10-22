# Enum members through a module's `our constant` alias

`Mod::Alias::Member`, where `Mod` declares `our constant Alias = Other::Enum`, now
resolves from an importing unit; the alias lives in the module's package symbols,
not the importer's env (#14174). KeyNub::LicDongle's `t/01-basic.rakutest` now
passes 7/7 like raku.
