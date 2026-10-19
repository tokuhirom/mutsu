# `$*SPEC.curupdir` answers a real Block

`IO::Spec::*.curupdir` used to answer an invented `IO::Spec::CurUpDir` instance that only a by-name
`ACCEPTS` gate understood, so smartmatch, `grep` and `.arity` on it were wrong. It now answers a
`Block` with Rakudo's signature and body (`-> str $dir { $dir ne '.' && $dir ne '..' }`), and the
stand-in class and its gate are gone.
