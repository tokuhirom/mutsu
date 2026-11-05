# The env-free declaration lane asks about the slot's own marker keys

`env_free_decl_slot` (the lean `my $x = <scalar>` lane, #12151) declined every
declaration once the program had created any sigilless readonly/alias marker
key (`my \z = 1` anywhere, as EC's `secp256k1` does), through two process-wide
latches, so each declaration in such a program took the full store cascade
(~1.2K instructions). It now asks the question it has: whether THIS slot's
marker key or alias key was ever an env key (`maybe_env_key`). The bigint
inverse loop in a program with a sigilless name went from 1.314M to 1.115M
instructions (1.023M with none); EC `t/secp256k1.t` ~21 s -> ~18.5 s (#15981).
