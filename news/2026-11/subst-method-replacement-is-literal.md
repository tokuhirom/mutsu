# `.subst` keeps a literal `$0` in its replacement string

`"abc".subst(/(b)/, '[$0]')` answered `a[b]c`: the method forms of `subst`
expanded `$0`, `$1`, ... in the replacement string after the match, which
Rakudo never does (`a[$0]c`; the replacement is the string the caller built).
`s///` and closure replacements still see the captures. The local backreference
test that pinned the old behaviour is corrected.
