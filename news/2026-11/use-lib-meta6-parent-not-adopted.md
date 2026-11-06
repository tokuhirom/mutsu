# A `use lib` directory no longer adopts its parent's `META6.json`

`resolve_module_from_meta6` read `<base>/../META6.json` for every `use lib` prefix, so `use lib 'dist/t'`
loaded the distribution's own module from `dist/META6.json` ahead of an earlier `use lib` entry that
provided the module. `raku` consults a `META6.json` only in the repository prefix itself (`-I dist`);
`-I dist/lib` answers "Could not find M". The lookup is narrowed to the prefix (#16822), which also
removes a module search location nobody named.
