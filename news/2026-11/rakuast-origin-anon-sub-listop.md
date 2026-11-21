# RakuAST: origins of a listop call in an anonymous sub

The parser marks the statements of an anonymous sub's body with no line, so
a listop call there (`sub { say 1 }`, `sub { print 1, 2 }`) got no
`.origin`, nor did its statement, name or argument list (#15780). Such a
call now runs from its name, written right before its first argument, to
the end of its last argument, when both are placed; its parts are derived
from that range, and the statement takes it (`listop_origins.rs`).
