# Internal pipes are close-on-exec

The signal self-pipe and the `run(:merge)` pipe were created with a raw `libc::pipe`, so every
child process inherited both ends. A child could write a signal number into the parent's signal
pipe and trigger its `signal()` taps, and a long-lived child kept the pipe open. Both now come
from one helper, `runtime::cloexec_pipe`, which uses `pipe2(O_CLOEXEC)` (and `fcntl(FD_CLOEXEC)`
where `pipe2` does not exist). The end handed to a child still arrives as its stdout/stderr,
because dup2 clears the flag on the target.
