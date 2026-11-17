# `$str ~~ Positional` is remembered per built-in type

A type smartmatch whose receiver is a built-in scalar (`Str`, `Int`, `Num`,
`Rat`, `Bool`, `Complex`, ...) went through the whole type-matching gauntlet on
every call. The per-(class, type) memo that #17331 added for user-class
instances did not admit a built-in receiver. A plain scalar has no MRO or roles
of its own, so whether it matches a nominal class or role is decided by its
built-in type. The memo now answers it per `(built-in type, type name)` for one
registry write generation. It covers the core roles, the catalog's core classes,
and the registered classes and roles. Types whose answer reads the value stay on
the full check: the native types, `UInt`, subsets and enums. So does a name that
something in scope re-binds. A remembered key re-asks only that scope-dependent
half.

The smartmatch path also stopped allocating on every call. Four `Symbol` →
`String` conversions were there only to compare a name. The user-`ACCEPTS`
probe re-looked-up a class it already held as a symbol.

Per iteration of `my $x = $v ~~ Positional` with `$v` a `Str` (callgrind,
excluding the loop's own store): 10.6k → 5.4k instructions, and with
`use SBOM::CycloneDX` loaded 12.4k → 6.8k. The extra cost a loaded program adds
to the smartmatch itself fell from ~1.8k to the noise; what remains is the
store of the result, filed separately. Refs #17876, #17811.
