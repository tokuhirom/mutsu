# A slang action's `meta-object` no longer judges the unit's classes undeclared

A `routine-declarator` action that compiles its block with `meta-object` while the unit is still
being parsed (Qwiratry's `mold ... do { Node.new }`) died "Undeclared name: Node" when the block
named a class the unit declares above it: the host evaluates the block in a context that does not
see the importing unit's classes. The undeclared-name check is now skipped inside a slang hook, as
the variable check already was, and the unit's own compile judges the names.
