# `.origin` of escaped double-quoted strings in regexes

A double-quoted string in a regex (`/ "a\n" b /`, `/^ "\e[34m# x\e[0m"/`)
ended the regex's origin walk as soon as it held a backslash. The quote and
every atom after it were left without a range. The walk now decodes the
single-character escapes (`\n`, `\t`, `\r`, `\e`, `\a`, `\b`, `\f`, `\0`, and
an escaped `\`, `"`, sigil or brace) and compares the result with the quote's
value. It places the quote as rakudo 2026.11 does. A string that interpolates
or uses another escape stays without a range (#15780).
