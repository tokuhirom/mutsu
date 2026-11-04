# `IO::Spec::Win32` reads a server without a share like rakudo

`//a`, `//a/` and `//a//b` were taken as a UNC volume with a share. `splitpath` now uses
rakudo's volume (`//server/share`, or `//server/` at the end of the path) and keeps its original
slashes; `canonpath`, `catdir`, `catfile` and `rel2abs` read `//server` as the volume `\\server`
and the separators after it as ordinary path separators (`//a//b` is `\\a\b`) (#16505).
