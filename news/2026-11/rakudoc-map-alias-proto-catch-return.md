# rakudoc: map listop aliasing, proto CATCH return, handle subclasses

The rakudoc suite now passes in full. Its `t/01-cmd.t` and `t/02-cache.t`
showed these differences from rakudo, all fixed:

- `map BLOCK, $a, $b` (and `grep`) with several list arguments keeps each
  argument's container, so the topic aliases it as `($a, $b).map(BLOCK)`
  does. rakudoc swaps `$*OUT`/`$*ERR` for capturing handles with
  `map { IO::MiddleMan.hijack: $_ }, $*OUT, $*ERR`. A dynamic variable
  declared in a caller frame is now aliased in a list literal too.
- A `return` in a proto's `CATCH`, handling an exception that its `{*}`
  candidate threw, returns from the proto. Before, the candidate (which
  shares the proto's name and so its callable id) took it as its own
  return, and the proto went on to its last statement. A return a `CATCH`
  raises now carries its installing routine's stack depth. Each routine
  frame decides by that depth whether the return is its own, which also
  works for a qualified `Pkg::proto` call.
- `IO::Handle.t` on a handle that was never opened is `False`. A subclass of
  `IO::Handle` now reaches `.t`, `.lock`, `.unlock` and `.spurt`.
- `CompUnit::PrecompilationRepository::Default.try-load` creates its store's
  prefix directory on first use, as rakudo does. No unit is stored.
