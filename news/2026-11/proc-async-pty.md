# `Proc::Async.new(:pty(:rows, :cols))` runs the child behind a pseudo-terminal

`Proc::Async` used to accept `:pty` and ignore it, so the child saw pipes (#17751). It now opens a
pseudo-terminal of the requested size and gives its slave side to the child as stdin, stdout and
stderr, in a session of its own with the slave as its controlling terminal, so `$*OUT.t` is true
there. The parent reads the master side as `.stdout`; stderr is merged into it, as rakudo documents.
Writes go to the master, with or without `:w`, as in rakudo. `.pty` reports whether the process
has one, `.resize-pty(:rows, :cols)` changes its size, and a bare `:pty` is refused with rakudo's
message.

Every fd made for it is close-on-exec, so the master never leaks into another child. The code is
Unix-only (`src/runtime/proc_pty.rs`): Linux names the slave with `ptsname_r` and macOS with
`TIOCPTYGNAME`; elsewhere `:pty` fails to start with an `X::OS`. Rakuish's `t/28-pty-exec.t` and
`t/29-pty-capability.t` now match rakudo.
