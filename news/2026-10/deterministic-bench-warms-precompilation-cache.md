# Deterministic benchmarks warm the precompilation cache

The deterministic instruction and allocation benchmark now runs each selected benchmark once in
both JIT modes before callgrind. Push-triggered runs therefore measure a populated precompilation
cache even when the wall-clock benchmark job is skipped.
