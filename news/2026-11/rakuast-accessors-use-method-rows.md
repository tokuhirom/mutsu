# RakuAST accessors use registered method rows

RakuAST field and derived accessors now use static method rows shared by
ordinary native nodes and user subclasses carrying a native node. This
removes the two direct accessor branches in the pure zero-argument entry.
The cache stores the selected row, so every call still reads the current node.

The 405 new rows use declaring owners measured with Rakudo 2026.09.
Reflection uses those owners too: an IntLiteral's value accessor belongs to
Literal, and compile-time-value for a Sub belongs to Routine. HashIndex no
longer advertises assignee, and original-blorst is limited to FIRST and POST.

Generated owner chains and accessor declarations are checked against the
model schema. Direct entry tests cover cached calls across nodes and subclass
payloads; Raku tests cover parsed fields, hand-built AST evaluation, declaring
owners, and absent methods. The existing scalar accessor complexity gap is
tracked by #16130.
