# `CORE::` lists the built-in routines

`CORE::.keys` / `.elems` were empty and `CORE::<&say>:exists` was False because `CORE::`
was read as an (empty) package literally named `CORE`. It is now the setting's stash:
every built-in routine that has a first-class `&name` value is listed, as in rakudo
(`&print`, `&say`, `&uc`, ...). Code that completes or inspects names, such as the
Jupyter::Kernel autocompleter (`CORE::.keys.grep: { /^ '&'? "prin" / }`), finds them.

`CORE::` still lacks the setting's types and `$?`-variables that rakudo lists; those are
not part of this change.

Found by running the Jupyter::Kernel distribution (`t/05-autocomplete.rakutest`).
