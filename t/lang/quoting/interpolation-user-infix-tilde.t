use Test;

# #15932: in the scope of a user `multi infix:<~>`, a string interpolation
# asks its candidates with the pieces as they are, like `"A: " ~ $a`.

plan 7;

{
    sub infix:<~>($a, $b) { "[$a|$b]" }
    is 'a' ~ 'b', '[a|b]', 'an explicit user concat still selects the user operator';
    is "left { 'a' ~ 'b' }", 'left [a|b]',
        'interpolation in a user concat body does not recursively call it';
}

{
    multi infix:<~>($a, $b) is default { 'user' }
    my $s = 'value';
    is "prefix $s", 'prefix value', 'core Str pair outranks a generic user multi';
}

{
    my class A { method Str { self } }
    multi infix:<~>(Str() $a, A $b) { "B" }
    my $a = A.new;
    is "A: $a", "B", 'interpolation picks the user candidate (Str answering self)';
}

{
    my class C { method Str { "c" } }
    multi infix:<~>(Str $a, C $b) { "<$a|" ~ "c>" }
    my $c = C.new;
    is "x$c", "<x|c>", 'candidate wins over stringifying the object';
    is "plain {1 + 1}", "plain 2", 'pieces no candidate takes stringify as before';
    is "$c", "c", 'a single piece is not a concatenation';
}
