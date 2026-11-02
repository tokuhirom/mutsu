# Stores in a program with a sigilless binding skip marker probes for other names

Once a program binds any sigilless name (`my \z = 1`), the readonly and alias
marker probes run for every scalar store. They now ask the per-name
"ever an env key" bit first, the declaration path no longer copies the env to
remove a marker key that is not there, and the tied-store check of a local
slot uses the slot's pre-interned symbols instead of interning the name and
building the marker key per store. A bigint inverse loop in such a program
dropped from 1.415M to 1.314M instructions (the same loop without a sigilless
binding anywhere: 1.026M). EC `t/secp256k1.t`: 29.6 s -> ~27.9 s (#15981).
