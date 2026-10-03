use Test;

# A class body statement that writes an outer lexical (`class F { $z = 4 }`)
# compiles to a package-qualified store. When another class's method has
# captured that lexical, the outer `$z` lives in a shared cell; the store must
# write THROUGH the cell, not replace it with the plain value, or the method
# keeps reading the old value (#11086). Likewise a class-body `:=` bind to an
# outer lexical must share its container in both directions.

plan 11;

{
    my $z = 1;
    class E1 { method m { $z } }
    class F1 { $z = 4 }
    is E1.new.m, 4, 'class-body write reaches a method that captured the outer lexical';
    is $z, 4, 'the outer lexical sees the class-body write';
    $z = 7;
    is E1.new.m, 7, 'a later outer write still reaches the method (cell not severed)';
}

{
    my $z = 1;
    class E2 { method m { $z } }
    my $inside;
    class F2 { $z = 4; $inside = E2.m }
    is $inside, 4, 'the write is visible to the method from inside the writing class body';
}

{
    my $z = 1;
    my $w-seen;
    class E3 { my $w := $z; $z = 5; $w-seen = $w }
    is $w-seen, 5, 'class-body bind alias sees a later write to its source';
    is $z, 5, 'the source keeps the written value';
}

{
    my $z = 1;
    my $z-seen;
    class E4 { my $w := $z; $w = 5; $z-seen = $z }
    is $z-seen, 5, 'a write through a class-body bind alias reaches the source';
    is $z, 5, 'the outer lexical sees the write through the alias after the body';
    $z = 9;
    is $z, 9, 'the outer lexical stays writable after the bind';
}

{
    my $z = 1;
    class E5 { my $w := $z; method m { $w } }
    $z = 8;
    is E5.m, 8, 'a method reading the class-body alias sees a later outer write';
}

{
    my $last = do { my $a = 3; my $b := $a; };
    is $last, 3, 'a block-final scalar bind still yields the bound value';
}
