# `shell(..., :out($handle))` writes the command's output to the handle

`shell` only knew `:out` as a request to capture; given an `IO::Handle` it sent the
child's stdout to `/dev/null`, so the file stayed empty (`run` redirected it correctly).
App::RaCoCo's `RunProc.run`, which calls `shell($command, :$out, ...)`, hit that in
`t/02-ext-proc.rakutest`. `run` and `shell` now share the helper that turns the handle into
the child's stdout (and, with `:merge`, its stderr).
