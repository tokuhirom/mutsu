# A `:D`/`:U` parameter no longer keeps a routine off the light call paths

`FastParamCheck` had no smiley form, so a routine with a `Str:D $s` or
`Int:D $n` parameter never qualified for the light / positional-light call
paths and every call ran the general binder. `Str:D` is the house style of
much of the ecosystem: MCP::Server::Tool::Web's tokenizer and renderer are
built from dozens of small `Str:D`/`Int:D` helpers.

A parameter whose base is a boxed concrete kind (`Int`, `Str`, `Num`, `Bool`,
`Rat`) now carries its smiley in `FastParamCheck::Fast`; the light bind checks
the base as before and then the smiley, which for these kinds is decided by
the value's shape alone (a type object, or a role mixed onto one, is
undefined). A failure raises the binder's own errors:
`X::Parameter::InvalidConcreteness` when only the smiley failed, and the
plain type-check failure, naming the base, otherwise. `Any:D`/`Mu:D`,
`UInt:D` and named smiley parameters keep the general binder.

MCP::Server::Tool::Web `html-to-text` on a mixed synthetic page: 11.6M ->
11.1M instructions per repetition (callgrind, profiling build). Refs #17385.
