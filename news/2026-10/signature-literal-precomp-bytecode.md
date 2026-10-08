# Signature literals now served from the compiled-bytecode cache

A module whose mainline holds a `Signature` literal is no longer refused by
the bytecode precompilation cache. A Signature carrying its `SigInfo` is
rebuilt under a fresh id on decode, so it holds no cross-process identity; its
recorded id and derived attributes are left out of the encoding so the verify
mode compares byte for byte. (#11841)
