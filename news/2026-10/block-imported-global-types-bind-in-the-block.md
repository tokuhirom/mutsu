# A block's imported package-less type is a pad term too

A type a module exports from outside any package (`class Foo is export`
in a file with no `unit module`) is now bound in the pad of the block that
`use`s the module, as packaged exported types already were (ADR-8751,
#14140). The compiler binds it like the others; `GetImportedVar` resolves
such a type by its own name. The import writes no env alias for it.

The visibility gate for a module's bare names (`bare_name_visible_here`)
now also accepts a name an open block's `use` binds in its pad, except in a
top-level routine the block calls. So `::('Foo')` in the block still finds
the type, and the same lookup in a routine written outside the block no
longer does. Before, the routine saw the type through the block's env
alias.
