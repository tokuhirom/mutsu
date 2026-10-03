# `%*ENV` writes no longer touch the process environment

`%*ENV` is now an ordinary hash, as in rakudo (#11241). A store or delete used
to be mirrored into the C-level process environment with `std::env::set_var` /
`remove_var`, which is not thread-safe against C code (NativeCall libraries,
the dlopen'd `libssl` / `libmysqlclient`, libc itself) reading the environment
on another thread, and which made a write visible to `getenv()` where rakudo's
is not.

The places that silently relied on the mirror now read the hash instead:

- `run`, `shell` and `Proc::Async` pass `%*ENV` (or an explicit `:env`) as the
  child's *whole* environment, so a variable deleted from `%*ENV` is no longer
  inherited from the process environment;
- `%*ENV<X>` / `%*ENV<X>:exists` no longer fall back to the process
  environment for a key missing from the hash, so a deleted key stays gone;
- `$*SPEC.path` (Unix and Win32) reads `%*ENV<PATH>`.

`t/io/proc-env-hash-is-child-environment.t` pins all of it against rakudo.
