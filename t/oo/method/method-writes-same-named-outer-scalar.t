use Test;

# #18274 slice 3: a method reaches a scalar of its declaring scope through
# that scope's cell, even when a class of the same spelling shares the
# scalar's sigil-less env key.

plan 5;

{
    my $C = 0;
    class C { method foo { $C } }
    is C.new.foo, 0, 'a method reads an outer $C, not the type C';
}

{
    my $Foo;
    class Foo { method bar { $Foo++ } }
    Foo.new.bar;
    Foo.new.bar;
    is $Foo, 2, 'a method increments an outer $Foo of its own class name';
}

my $K = 10;
class K { method bump { $K += 5 } }
K.new.bump;
is $K, 15, 'a top-level $K written by a method of class K';

{
    my $D = 0;
    class D2 { method foo { $D } }
    class D { }
    is D2.new.foo, 0, 'a later class D does not hide $D from an earlier method';
}

{
    my $W = 1;
    class W { method w { $W = $W * 3 } }
    W.new.w;
    W.new.w;
    is $W, 9, 'repeated writes accumulate in the outer $W';
}
