# A module's imported enum value no longer answers a same-named scalar read

Inside a `unit module`, an imported enum value named `literal` answered every read of a `$literal`
parameter in a `.map` callback (`-> $literal { $literal }` returned `AST-Types::literal`). The
module-scope tables that serve a module's own routines recorded an imported enum key under its bare
name, the one key a scalar of that name is read by. They now keep it in the enum-key namespace, and
the module's bareword reader probes that namespace, so the enum value is still what the bareword
`literal` means. This fixes PDF's `ast-coerce` round trip (#14255).
