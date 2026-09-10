use Test;
plan 12;

# ss/// — samespace substitution (preserves whitespace from matched text)
$_ = "a\nb\tc d";
ok ss/a b c d/w x y z/, 'ss/// returns truthy on success';
is $_, "w\nx\ty z", 'ss/// preserves original whitespace';

# s:ss/// — explicit :ss adverb
$_ = "a\nb\tc d";
ok s:ss/a b c d/w x y z/, 's:ss/// returns truthy on success';
is $_, "w\nx\ty z", 's:ss/// preserves original whitespace';

# s:s/// — :s (sigspace) without samespace, does NOT preserve whitespace
$_ = "a\nb\tc d";
ok s:s/a b c d/w x y z/, 's:s/// returns truthy';
is $_, "w x y z", 's:s/// does not preserve whitespace';

# s[pattern] += value — compound assignment substitution
$_ = "a 2 3";
s[\d] += 5;
is $_, "a 7 3", 's[\\d] += 5 applies addition to matched digit';

# s:g[pattern] x= n — global compound assignment with string repeat
$_ = "a b c";
s:g[\w] x= 2;
is $_, "aa bb cc", 's:g[\\w] x= 2 repeats each match twice';

# s: is always substitution even when sub s is declared
{
    sub s { 'sub s' }
    $_ = "foo";
    ok s:s,foo,bar, , 's with colon is always substitution even when sub s exists';
    is s(), 'sub s', 'can call sub s as "s()"';
    is s, 'sub s', 'can call sub s as "s"';
    is $_, 'bar', 'substitution worked correctly';
}
