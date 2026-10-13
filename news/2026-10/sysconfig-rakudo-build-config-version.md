# `Perl6::SysConfig.rakudo-build-config<version>` is the Rakudo release coordinate

`nqp::gethllsym("default","SysConfig").rakudo-build-config<version>` answered mutsu's crate version (`0.25.0`),
which `Rakudo::CORE::META` reads as "the Rakudo version" (and Rakudo answers `2026.07` there, as the issue that added
the op expected). It now answers `Interpreter::RAKUDO_COMPAT_VERSION`, the same coordinate
`$*RAKU.compiler.version` reports (ADR-0104). The distribution stays red on the shape of the `core` repository's
distribution (#13268).
