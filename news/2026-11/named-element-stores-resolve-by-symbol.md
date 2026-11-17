# Named element stores resolve their target by symbol

An element store whose target is not one of the running code's own slots
takes the general store body. That covers a hash or array captured by a
nested `my sub`, an `our` variable, and a sigilless alias. MCP::Server::Tool::Web's
tokenizer runs `%memo{$needle} = [$from, $at // Int]` about 264k times per test
file in this shape.

The store body consulted its target through `place_env_get(&str)` and its
siblings, ~60 call sites, and every call interned the variable name again: 21
interns for that one store. The body now takes the target's symbol once, the
chunk's constant-pool symbol or one intern for an alias target, and the place
accessors probe the env by symbol. The `&str` form of `place_get` is gone.

`%memo{$k} = [$a, 4]` from a nested sub went from 40.4k to 36.8k instructions
(callgrind slope, profiling build). The rest of its gap to a local hash (~11k)
is that a captured hash never takes the plain-hash fast lanes. Their "another
holder exists" test is a reference count, and a capture always adds holders.
