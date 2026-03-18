use Test;

plan 6;

# qq:!c disables closure interpolation
{
    my $x = "world";
    is qq:!c/hello $x {1+2}/, 'hello world {1+2}', "qq:!c disables closure interpolation";
}

# qq:!a disables array interpolation
{
    my @arr = 1, 2, 3;
    my $s = "test";
    is qq:!a/$s @arr[]/, 'test @arr[]', "qq:!a disables array interpolation";
}

# qq:!a:!c disables both array and closure interpolation
{
    my $x = "val";
    is qq:!a:!c/$x {block} @arr/, 'val {block} @arr', "qq:!a:!c disables array and closure";
}

# q:s:!c enables scalar but disables closure
{
    my $v = "hi";
    is q:s:!c/$v {block}/, 'hi {block}', "q:s:!c enables scalar but disables closure";
}

# Q:s enables only scalar interpolation
{
    my $v = "there";
    is Q:s/$v @arr {block}/, 'there @arr {block}', "Q:s enables only scalar interpolation";
}

# q with negated adverb should work
{
    is q:!c/hello/, "hello", "q:!c is just like q";
}
