# The parser expands the bundled search paths only when it resolves a module

The bundled batteries' scan (a `read_dir` of the bundle and a `stat` of each
distribution's `lib/`) had moved out of `Interpreter::new`, yet every run
still paid for it. Before each parse the runtime handed the parser its search
paths as a flat list, its own lib paths plus the scanned bundle, so even
`say "hello"` scanned the bundle.

The scan now lives below the runtime (`bundle_paths`). The parser keeps its
paths as three parts: the runtime's lib paths, whether the bundle follows
them, and the parse's own `use lib` additions. It expands the bundle only
when it resolves a module (`find_module_file`, a slang activation, a `use
lib` that might name a bundled directory). The search order is unchanged.

Refs #19197.
