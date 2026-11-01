# A method's positional parameter is visible to declaration-time readers in its body

The fast method binder left a plain positional parameter out of `env` (it is read
through its slot), so a grammar's token code block, an assertion block or an
attribute default of a class declared in the method body read `Any` for it. The
binder now keeps a parameter in `env` when the frame's `needs_env_sync` says a
by-name reader can read it (`CompiledCode::local_needs_env_sync`). Net::BGP
`t/02-as-list.t` (`AS-List.from-str`) now matches rakudo, 39/39 (#15863).
