# `my \x = do { ... }` binds the block tail's container

A sigilless declaration whose initializer is a `do` block or `do EXPR` now
binds the container of the block's tail, like the parenthesized spelling
already did. `my \c = do { Proxy.new(FETCH => ..., STORE => ...) }; c = 4`
calls `STORE` as on rakudo instead of dying with `Cannot modify an immutable
Int`. This unblocks Text::CodeProcessing's REPL sandbox (`my \_ = do { ...
Proxy.new(...) }`).
