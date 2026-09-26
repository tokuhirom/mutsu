# Version::Raku comparison methods run on mutsu

mutsu now supports `Version::Raku`'s BEGIN-time installation of builtin comparison routines as methods. Version subclasses also delegate native payload comparisons, `ACCEPTS`, and version introspection to their underlying `Version` value. The distribution's two test files now pass completely under both mutsu and Rakudo.
