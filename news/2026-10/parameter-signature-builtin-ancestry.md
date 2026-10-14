# Parameter and Signature have a builtin type ancestry

The builtin type catalog had no rows for `Parameter` and `Signature`, so a value of either type had
no ancestors for multi dispatch: `Mu` and `Any` candidates scored the "unrelated" distance and landed
in the narrowest tier ahead of a nominally exact candidate whose sibling `where` clause had failed.
MCP::Server's DSL type map (`multi type-map(Parameter $par)` next to `multi type-map(Mu $_ where ...)`)
died with "Type Parameter can not be auto converted". Both types now follow `Any` and `Mu`.
