use Test;

# A write through the `OUTER::` pseudo-package that names the binding a plain
# `$x` sees is a write to that `$x`: a `:=` makes the outer lexical an alias of
# the source container, so a later write to the source shows through (#10676,
# #10675). It used to be stored under a literal `OUTER::x` key and lost.

plan 14;

{
    my $x = 1;
    sub s1 { my $y; $OUTER::x := $y; $y = 5 }
    s1();
    is $x, 5, 'sub: $OUTER::x := $y aliases the outer $x (predeclared source)';
}

{
    my $x = 1;
    sub s2 { $OUTER::x := my $y; $y = 5 }
    s2();
    is $x, 5, 'sub: $OUTER::x := my $y aliases the outer $x (declaration source)';
}

{
    my $x = 1;
    { my $y; $OUTER::x := $y; $y = 5 }
    is $x, 5, 'bare block: predeclared source';
}

{
    my $x = 1;
    { $OUTER::x := my $y; $y = 5 }
    is $x, 5, 'bare block: declaration source';
}

{
    my $x = 1;
    my &c = { my $y; $OUTER::x := $y; $y = 4 };
    c();
    is $x, 4, 'closure: the rebind reaches the captured outer $x';
}

{
    my $x = 1;
    sub s3 { my $z; ($OUTER::x := $z); $z = 9 }
    s3();
    is $x, 9, 'expression-position bind';
}

{
    my $x = 1;
    sub s4 { my $y; $OUTER::x := $y; $y = 5; $OUTER::x ~ '/' ~ $x }
    is s4(), '5/5', 'the rebound outer $x reads back through both spellings';
    $x = 6;
    is $x, 6, 'the rebound outer $x stays assignable';
}

{
    my $x = 1;
    sub s5 { $OUTER::x = 7 }
    s5();
    is $x, 7, 'plain assignment through $OUTER::x';
}

{
    # A distinct name: a same-named sibling rebind trips #10826.
    my $c = 1;
    sub s6 { $OUTER::c++; ++$OUTER::c; $OUTER::c--; --$OUTER::c; $OUTER::c += 10 }
    s6();
    is $c, 11, 'increments and compound assignment through $OUTER::c';
}

{
    my @a = 1, 2;
    sub s7 { my @b = 3, 4; @OUTER::a := @b; @b.push(5) }
    s7();
    is-deeply @a, [3, 4, 5], '@OUTER::a := @b aliases the outer array';
}

{
    my %h = a => 1;
    sub s8 { my %g = b => 2; %OUTER::h := %g; %g<c> = 3 }
    s8();
    is-deeply %h, %(b => 2, c => 3), '%OUTER::h := %g aliases the outer hash';
}

{
    my $x = 1;
    { my $x = 2; { my $y; $OUTER::x := $y; $y = 5 }; is $x, 5, 'OUTER:: names the nearest $x' }
    is $x, 1, 'the shadowed outermost $x is untouched';
}
