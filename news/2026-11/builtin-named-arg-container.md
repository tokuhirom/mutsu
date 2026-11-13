# A built-in gets the plain value of a `:name($var)` argument at an import call site

Once a program imports routines (`use NativeCall` is enough), a call to a
built-in compiles to a lexical-import call site, which passes `:name($var)`
as a Pair holding `$var`'s container so that an imported routine with an
`is rw` named parameter could write through it. When the call fell through
to the built-in, that container reached it unchanged: `dir($path,
:test($regex))` smart-matched every entry against the container and returned
nothing. The arguments a built-in receives are now normalized the same way
for named arguments as they already were for positional ones. The Readline
distribution's library probe now finds `libreadline.so.8`, and its
`t/02-base.t` passes 17/17 like rakudo.
