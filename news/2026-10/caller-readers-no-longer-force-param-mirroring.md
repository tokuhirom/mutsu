# A reader of the immediate caller no longer makes every routine mirror its parameters

Until now one `CALLER::<$x>`, bare `CALLER::` or `&CALLER::LEXICAL::(..)` anywhere in the
program (`Test.rakumod` has two) latched the process-wide flag that makes every light call write
each parameter into the frame env by name. `use Test` therefore cost every call of a small sub
an env write, about 1004 `env_deep_copies` per 1000 calls of `sub f($n)` (#14337).

These readers only ever look at the immediate caller. They use a separate latch now, and a light
frame that skipped its parameters' env write is recorded in `Locals`; when a callee pushes its
`caller_env_stack` entry (`push_caller_env`) the recorded frame's slot-only parameters are copied
into that entry. Deeper (`CALLER::CALLER::`) and cascading readers keep the old latch.
`t/routines/signature/caller-lexical-sees-light-caller-params.t` pins the visibility.

Measured: `use Test` + 1000 calls of `f($s)`: `env_deep_copies` 1004 -> 4.
