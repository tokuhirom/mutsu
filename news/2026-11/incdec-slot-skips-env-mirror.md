# `$i++` on a local no longer keeps the local's env mirror current

`compute_needs_env_sync` counted every `++`/`--` as a by-name access of its
variable, because the op's slow path (a Str, a Rat, an overflow, an undefined
start) read the variable from `env`. So every `$i++` of a slotted local also
wrote the value into `env` -- a tier insert and a copy-on-write probe on
every step of every counting loop.

A `++`/`--` that carries its slot now reads the slot on its slow path too
(after the anonymous-state, `our` and package-lexical stores, which answer
for names whose slot is only a mirror), so only a slotless step still marks
its variable as read by name.
