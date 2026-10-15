# A plain `@`/`%` store no longer mirrors into the env

Assigning to a plain `my @a` / `my %h` no longer copies the container into the
frame's name-keyed env on every store when the slot is authoritative: the same
rule a plain `$scalar` store has followed since the slot campaign (ADR-0039
§4.2's third bullet, #9895). Now that closures read free containers by upvalue
(#13581), the mirror only fed by-name readers, and the skip keeps it for the
cases that still have one (binds, constants, `needs_env_sync` consumers, a
program with reflective name access).
