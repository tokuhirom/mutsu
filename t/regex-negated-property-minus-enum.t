use Test;

plan 6;

# `<-:C-[:;,"]>` — a negated Unicode property (`:C` control) combined with a
# top-level `-[...]` enumerated subtraction. mutsu previously dropped the
# `-[...]` term, so the class matched the excluded characters (`: ; , "`).

grammar T {
    token safe  { <-:C-[:;,"]>+ }
    token a     { <-:C-[:;,]>+ }
}

# The enumerated exclusion is honoured: matching stops at the excluded char.
is T.subparse('5:Bubba', :rule<safe>).Str, '5', 'excludes ":" after a property term';
is T.subparse('work;x', :rule<safe>).Str, 'work', 'excludes ";" after a property term';
is T.subparse('a,b', :rule<safe>).Str, 'a', 'excludes "," after a property term';
is T.subparse('x"y', :rule<safe>).Str, 'x', 'excludes quote after a property term';

# A full anchored parse: the whole string is safe (no excluded chars, no control).
ok T.parse('HelloWorld', :rule<safe>), 'plain word matches the negated combined class';

# The quoteless variant behaves identically.
is T.subparse('5:Bubba', :rule<a>).Str, '5', 'property + enum subtraction (no quote in set)';
