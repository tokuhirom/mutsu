# `slurp(%?RESOURCES<file>)` reads the resource

The routine form of `slurp` took a `Distribution::Resource` for a plain string and
tried to open "Distribution::Resource<N>"; only the `.slurp` method worked.
`resolve_io_arg_path` now treats a resource like the IO::Path it shares its rows
with, so LLM::Prompts (`slurp(%?RESOURCES<prompt-stencil.json>)`) loads and its four
baseline files match rakudo.
