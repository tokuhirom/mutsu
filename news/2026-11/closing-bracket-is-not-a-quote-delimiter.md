# A closing bracket after `s`, `m`, `y`... is not a quote delimiter

`k(s)` where `s` is a callable parameter or lexical used to be read as the start of a
substitution `s)...)`, which swallowed the following statements and died with "Unrecognized regex
metacharacter ;" at run time. `)`, `]` and `}` never open a quote construct now, so such a bareword
is an ordinary term, as in rakudo. (#17176)
