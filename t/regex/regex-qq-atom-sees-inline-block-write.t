use Test;

# A `"..."` atom reads its variables when the match reaches it, so a write an
# earlier inline code block of the same match made is visible (#18630).

plan 8;

{
    my $s = "a";
    is ~("xb" ~~ / x { $s = "b" } "$s" /), "xb", 'qq atom sees an inline block assignment';
}
{
    my $u = "a";
    is ~("xb" ~~ / x { $u = "b" } "{ $u }" /), "xb", 'qq block atom sees an inline block assignment';
}
{
    my $c = 0;
    is ~("x1" ~~ / x { $c++ } "$c" /), "x1", 'qq atom sees an inline block increment';
}
{
    my $n = 0;
    "aaa" ~~ / [ "{ $n++; 'a' }" ]+ /;
    is $n, 4, 'a quantified qq atom runs once per attempt';
}
sub in-closure {
    my $s = "a";
    my &k = -> { ~("xb" ~~ / x { $s = "b" } "$s" /) };
    (k(), $s)
}
is-deeply in-closure(), ("xb", "b"), 'inside a closure the write reaches both the atom and the frame';
{
    my $q = "a";
    my $r = rx/ x { $q = "b" } "$q" /;
    is ~("xb" ~~ $r), "xb", 'a stored regex sees its own inline block write';
    is $q, "b", 'the write is kept after the match';
}
{
    my $w = "a";
    is ~("xa" ~~ / x <?{ $w eq "a" }> "$w" /), "xa", 'a read-only code assertion leaves the atom alone';
}
