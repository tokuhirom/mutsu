use Test;

# #15932: in the scope of a user `multi infix:<~>`, a string interpolation
# asks its candidates with the pieces as they are, like `"A: " ~ $a`.

plan 4;

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
