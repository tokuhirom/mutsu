# `my $x = [...]` and `my $o = C.new` take the lean declaration store

A `my $x = EXPR` declaration whose slot is the variable's only home is stored
by a lean lane that writes the slot and nothing else (#12151). That lane only
accepted an inert scalar: a number, a string, a type object, a pair. Any other
value went through the general store cascade, about 4k instructions per
declaration. That included an Array or Hash read out of a data structure
(`my $known = %memo{$needle}`), a list a routine returned
(`my $parsed = read-attributes(...)`) and a freshly constructed object.

The plain `$x = v` store already handles those values. It writes them with the
`$` container's itemization: an Array becomes an item Array, a Hash an item
Hash, and both still share the source object. The declaration lane now accepts
the same set of values and applies the same itemization. A container, Proxy,
Seq, lazy list, Slip or `Nil` still takes the general store, as before.

In MCP::Server::Tool::Web's tokenizer, 43k of its 98k declaration stores had
taken the general store.
