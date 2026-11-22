# Subscript-adverb names are ordinary keys in an extended variable name

`my $y:k[1,2] = 3; say $y:k<1 2>` failed with "Variable '$y' is not
declared": the use-site variable-name rule stopped before a colonpair whose
key was a subscript-adverb name (`k`, `v`, `kv`, `p`, `exists`, `delete`), so
`$y:k<1 2>` parsed as `$y` followed by a pair. Rakudo keeps every colonpair
in the name — `$a:delete` is the variable `$a:delete`, and a subscript adverb
only follows a subscript (`%h<a>:delete`) — so the use site now does the
same and agrees with the declaration (#18029).
