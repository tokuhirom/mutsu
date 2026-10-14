# `slurp($proc.err)` reads the pipe

`IO::Pipe` was registered without a parent, so `$proc.err ~~ IO::Handle` was False and the
`slurp` routine, which only treated an exact `IO::Handle` as a stream, stringified the pipe
and tried to open a file called `IO::Pipe<19>`. `IO::Pipe` now has `IO::Handle` as its
parent and `slurp` accepts any `IO::Handle` (a pipe, or a user subclass). App::RaCoCo's
`t/06-precompiler.rakutest` now gives the same output as Rakudo.
