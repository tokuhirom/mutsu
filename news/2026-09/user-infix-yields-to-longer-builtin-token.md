# A user single-character infix no longer steals the first half of `+&`, `**`, `//`, ...

Ecosystem `EC` (`lib/ed25519.rakumod`) declares
`multi sub infix:<+>(Point $a, Point $b) returns Point { ... }` and a few lines
later writes `$s[0] +&= 0b1111_1000;`. mutsu failed to parse the module with this
#7988 cluster's generic `Confused. expected statement: ... expression after infix
operator ...` message: the user-declared-infix matcher
(`match_user_declared_infix_symbol_op`) accepted the `+` of `+&=` as the user's
`infix:<+>` and left a stray `&=` behind.

Raku resolves this by longest-token matching, so a built-in infix whose first
character is the user's single-character operator always wins. The matcher
already refused a single-character op in front of `->`, `++`/`--` and a
compound-assignment `=`; it now also yields to the longer built-in infixes that
start with the same character: the numeric, string and boolean bitwise
operators (`+& +| +^ +< +>`, `~& ~| ~^ ~< ~>`, `?& ?| ?^`) and `**`, `//`, `~~`.
The user operator itself keeps working.

Pinned by `t/grammar/user-infix-longest-token-builtins.t`. `EC`'s
`ed25519` module now parses; loading it next stops at a run-time gap in the
lexically imported `FiniteField` arithmetic, which is outside this parse
cluster.
