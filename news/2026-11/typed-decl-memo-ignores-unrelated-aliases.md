# `my int $x` keeps its per-site memo in programs that import package aliases

The per-site memo that a plain builtin constraint (`my int $x`, `my Int $n`)
resolves to itself was switched off whenever the program's package alias table
was non-empty -- which it is as soon as any loaded module exports a nested type
under a short name. Red, for one, does, so every typed declaration in a Red
program re-ran the full constraint resolution.

The alias step renames a spelling only when some package aliases exactly that
name, and `package_type_alias` already answers `None` for a name outside the
table's alias-key union. The memo's gate now asks that union for the
constraint's own name instead of whether the table is empty.

Callgrind, release build: `Red::Regex::Unicode::property-ranges('Lu')` (a
1.1M-iteration native-int loop that declares `my int $matches` per iteration)
went from 17.13G to 16.12G instructions (-5.9%). Refs #15498.
