# Types declared in `package EXPORT::DEFAULT` are exported

A module whose body is `my package EXPORT::DEFAULT { class IP { ... } subset
IPVersion ...; our sub ... }` (Net::IP::Parse) exported its subs but not its
types: `use`ing it died with `Undeclared name: IP`, and `class IP is export`
there imported the name as `Any`
([#16332](https://github.com/tokuhirom/mutsu/issues/16332)). Rakudo makes the
stash's contents the tag's export list, so every type declared directly in it
is imported under its short name.

Two halves: the importer's parse-time module scan (`DeclScan::export_stash_type`)
records a class/role/subset/enum declared directly in an export stash as
exported under the stash's tag, so `my IP $x` parses; and the module load
collects those types (`ModuleLoadFacts::export_stash_types`) and, after the body
ran, binds each short name to the type registered as `[Unit::]EXPORT::<tag>::Name`.
The `is export` marker no longer files a variable export under the stash
package, which used to overwrite the binding with `Any`.

Verified on the real distribution: Net::IP::Parse 0.0.6 `t/basic.rakutest` goes
from 25 ok / 26 not ok to 102 ok, 0 not ok, matching `raku`.
Regression test: `t/modules/import-export/export-default-stash-types.t`
(fixture `t/lib/ExportDefaultStashTypes.rakumod`).
