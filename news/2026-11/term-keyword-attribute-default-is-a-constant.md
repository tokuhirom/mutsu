# `has Bool $.x = False` no longer costs every construction a scope collapse

A compunit that imports through a `sub EXPORT` hook compiles the CORE term
keywords `True`, `False` and `Nil` to a run-time check, because the hook may
have installed one of those names (#9047). JSON::Fast has such a hook, so any
module that `use`s it is affected. In that compunit, an attribute default such
as `has Bool $.self-closing = False` stops being a literal. Two things followed
from that, on every `.new`:

- the constructor evaluated the default;
- the constructor lane collapsed the caller's scoped environment first, in
  case the default read one of the caller's lexicals.

MCP::Server::Tool::Web's tokenizer builds one `Token` per tag. Each one paid
about 11k instructions for that collapse.

Unless a hook actually installed the name, the default can only produce the
constant it folds to. `Interpreter::decl_arg_constant` now answers it as that
literal, whether the default is held as an expression (a class attribute) or as
a compiled chunk. When a hook did install `False`, the default is still
evaluated and reads the hook's value. The set of hook-installed names only ever
grows, so an answer can never go stale.

On a profiling build, the tokenizer's callgrind slope dropped by 1.9%, from
3.04G to 2.98G instructions per 0.1 of the fixture. The work is part of #16897.
