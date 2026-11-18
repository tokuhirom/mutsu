# A slang module loads once, when its `use` is parsed

`use Slang::Tuxic` (or any module that registers a slang through Slangify or
`$*LANG.define_slang`) used to run the module twice: once in a separate
activation interpreter at parse time, to learn which grammar rules the slang
overrides, and once more when the program ran. Its mainline output, file
writes and load time happened twice.

The interpreter that runs the program now loads the module when the parser
reaches the `use`, as rakudo does, with the parse's `$*LANG` bound, so the
module's `EXPORT` hook registers the slang for the rest of the parse exactly
once. The `use` in position then only imports. This is slice 4a of
ADR-17558. Slangs whose own productions run during the parse (a slang host,
ADR-15762) still use the activation interpreter until slice 4b.
