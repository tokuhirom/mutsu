# One `PROCESS::<...>` read or one `subset` no longer slows every `my` declaration

Two process-wide facts kept every plain `my $x = ...` of a program off the lean declaration lane
(the slot-only store, ~1K instructions less than the env-mirroring one):

- `OpCode::GetPseudoStash` / `GetPseudoStashKeyed` were counted as "may read another frame's lexical
  by name", which latches `NON_EVAL_REFLECTIVE_SEEN` and makes every chunk mirror its locals into the
  env. `PROCESS::` reads the process's dynamic variables (`$*x`), which are never slot-only, so it is
  no longer one of them. Cro's `UnhandledErrorReporter` has one, so loading Cro latched it for the
  whole process.
- `native_typed_store_is_identity` declined for every store once any subset existed in the process,
  before it looked at the slot's own declaration. A slot declared with no constraint
  (`DeclaredConstraint::Untyped`) has nothing a subset could redirect, so it is answered first.

100000 iterations of `my $s = "abc" ~ $_` after `use Cro::HTTP2::RequestParser`, release build:
~150 ms -> ~93 ms (no module: ~66 ms); `my $s = "a{$_}b"`: ~165 -> ~115 ms (no module: ~85 ms).
Pinned by `t/types/enum-subset/untyped-my-in-loop-with-subset-and-process-stash.t` (#9170).
