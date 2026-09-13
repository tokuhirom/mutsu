`my` classes declared inside a `module`, `package`, or class body no longer
leak through that package's qualified name after the declaration scope ends.
Qualified inheritance parents also resolve to the existing package type when a
lexical class shadows the same source-facing name.
