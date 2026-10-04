# `<(` and `)>` inside a capture group narrow the group

```raku
my $m = "xab" ~~ /(a )> b)/;
say $m.Str;      # ab
say $m[0].Str;   # a
```

mutsu used to refuse this regex with "Unrecognized regex metacharacter >"
(#11570). The runtime parser's group scanner read the `)` of `)>` as the
group's closing paren, which left a stray `>`. A `<(` inside a group also
threw the scanner's paren count off. The scanner now reads both markers as
tokens.

Rakudo treats a capture group as a Match of its own, so the markers inside it
narrow the group's capture and leave the whole match alone. Once the regex
parsed, mutsu applied them to the whole match instead. Now the compiled
engine gives a capture group whose body sets a marker a capture level of its
own, and the group's span is narrowed to the markers. This works the same
for a named group (`$<x>=(a )> b)`) and for each iteration of a quantified
group.
