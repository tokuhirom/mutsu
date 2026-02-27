use Test;
plan 9;

{
    my Int @a;
    is @a.WHAT, '(Array[Int])', 'typed array WHAT keeps element type';
    is @a[0].WHAT, '(Int)', 'typed array missing element defaults to type object';
}

{
    my Int %h;
    is %h.WHAT, '(Hash[Int])', 'typed hash WHAT keeps value type';
    is %h<a>.WHAT, '(Int)', 'typed hash missing key defaults to value type object';
}

{
    my %h{Str} of Int;
    is %h.WHAT, '(Hash[Int,Str])', 'typed hash with key type keeps both constraints';
}

{
    my $a = Array[Int].new;
    is $a.WHAT, '(Array[Int])', 'Array[Int].new carries parametric WHAT';
    is $a[0].WHAT, '(Int)', 'Array[Int].new missing element defaults to Int';
}

{
    class A is Array {}
    class H is Hash {}
    my $a = A[Int].new;
    my $h = H[Int].new;
    is $a.WHAT, '(A[Int])', 'parametric array subclass WHAT is preserved';
    is $h.WHAT, '(H[Int])', 'parametric hash subclass WHAT is preserved';
}
