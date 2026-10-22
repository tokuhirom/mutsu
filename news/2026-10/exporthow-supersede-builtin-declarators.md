# EXPORTHOW::SUPERSEDE replaces the HOW of built-in `class` and `role`

A `use`d module's `my package EXPORTHOW { package SUPERSEDE { constant class = SomeHOW } }` is now read
at parse time (next to `DECLARE`) and every `class` / `my class` / `unit class` / anonymous `class { }`
and `role` declaration of the importing unit is built with that HOW. AttrX::Mooish's unmodified
`t/lib/CustomHOW.rakumod` now yields `CustomClassHOW` / `CustomRoleHOW` like rakudo. Fixes #14241.
