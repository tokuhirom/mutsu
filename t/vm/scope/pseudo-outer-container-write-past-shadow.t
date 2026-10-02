use Test;

# `@OUTER::a` / `%OUTER::h` name a container of an enclosing scope even when a
# scope in between declares its own `@a` / `%h`: a whole-container store, a
# mutating method call and an element store all reach it, in the same frame and
# across a routine boundary. They used to be lost, and a read of `@OUTER::a`
# was not even lexical (#10857).

plan 20;

# --- whole-container store ---

{
    my @a = 1;
    { my @a = 2; @OUTER::a = 3, 4; is @a, [2], 'block: the shadow array is untouched' }
    is @a, [3, 4], 'block: @OUTER::a = list past an inner @a';
}

{
    my @b = 1;
    sub s1 { my @b = 2; @OUTER::b = 3, 4; @b }
    is s1(), [2], 'sub: the sub\'s own @b is untouched';
    is @b, [3, 4], 'sub: @OUTER::b = list past the sub\'s own @b';
}

{
    my %h = a => 1;
    { my %h = x => 9; %OUTER::h = b => 2; is %h, {x => 9}, 'block: the shadow hash is untouched' }
    is %h, {b => 2}, 'block: %OUTER::h = list past an inner %h';
}

{
    my %g = a => 1;
    sub s2 { my %g; %OUTER::g = c => 3 }
    s2();
    is %g, {c => 3}, 'sub: %OUTER::g = list past the sub\'s own %g';
}

{
    my @f = 1;
    sub s3 { my @f = 2; @OUTER::f := [9, 9] }
    s3();
    is @f, [9, 9], 'sub: @OUTER::f := [...] rebinds the outer @f';
}

# --- reads ---

{
    my @c = 1;
    { my @c = 2; is @OUTER::c, [1], 'block: @OUTER::c reads the outer @c' }
    sub s4 { my @c = 2; @OUTER::c }
    is s4(), [1], 'sub: @OUTER::c reads past the sub\'s own @c';
    my %k = a => 1;
    { my %k; is %OUTER::k, {a => 1}, 'block: %OUTER::k reads the outer %k' }
}

# --- mutating method calls ---

{
    my @c = 1;
    { my @c = 2; @OUTER::c.push(5) }
    is @c, [1, 5], 'block: @OUTER::c.push past an inner @c';
}

{
    my @d = 1;
    sub s5 { my @d; @OUTER::d.push(6) }
    s5();
    is @d, [1, 6], 'sub: @OUTER::d.push past the sub\'s own @d';
}

{
    my @e = 3, 1, 2;
    { my @e; @OUTER::e.=sort }
    is @e, [1, 2, 3], 'block: @OUTER::e .= sort';
}

{
    my $x = 1;
    { my $x = 2; $OUTER::x.=succ; is $x, 2, 'block: .= leaves the shadow scalar alone' }
    is $x, 2, 'block: $OUTER::x .= succ past an inner $x';
}

{
    my $x = 1;
    { my $x = 2; $OUTER::x = $OUTER::x.succ }
    is $x, 2, 'block: $OUTER::x = $OUTER::x.succ past an inner $x';
}

# --- element stores ---

{
    my @a = 1, 2;
    { my @a = 0; @OUTER::a[0] = 9 }
    is @a, [9, 2], 'block: @OUTER::a[0] = v past an inner @a';
}

{
    my %h = a => 1;
    { my %h; %OUTER::h<b> = 2 }
    is %h, {a => 1, b => 2}, 'block: %OUTER::h<k> = v past an inner %h';
}

{
    my @a = 1, 2;
    sub s6 { my @a; @OUTER::a[1] = 7 }
    s6();
    is @a, [1, 7], 'sub: @OUTER::a[1] = v past the sub\'s own @a';
}
