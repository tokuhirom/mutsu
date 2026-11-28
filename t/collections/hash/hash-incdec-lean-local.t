use Test;

# `%h{$k}++` on a `%` the running code declares takes the lean step through
# the variable's slot. These pin the shapes it must decline, and that what it
# accepts answers what rakudo does.

plan 14;

sub counts(@words) {
    my %c;
    %c{$_}++ for @words;
    %c
}
is-deeply counts(<a b a c a>), %(a => 3, b => 1, c => 1), 'a sub-local word count';

{
    my %h;
    my $r = %h<k>++;
    is $r.WHAT, Int, 'postfix ++ on an absent key yields an Int';
    is $r, 0, '... of 0';
    is (++%h<k>) + 10, 12, 'prefix ++ is usable in an expression';
}

{
    my %h = a => False;
    %h<a>++;
    is %h<a>, True, 'a Bool element takes the generic step';
}

{
    my %h = a => 5;
    my $x = 1;
    %h<b> := $x;
    %h<b>++;
    is $x, 2, 'a bound element steps the variable it is bound to';
    %h<a>--;
    is %h<a>, 4, 'a plain element beside it steps in place';
}

{
    my %h = a => 1;
    my %b := %h;
    %h<a>++;
    %b<a>++;
    is %h<a>, 3, 'a := alias of the hash sees both steps';
}

{
    my %h = a => -9223372036854775808;
    %h<a>--;
    is %h<a>, -9223372036854775809, 'underflow past int64 promotes to a big Int';
}

{
    my %h;
    %h{$_ % 3}++ for ^9;
    is-deeply %h, %('0' => 3, '1' => 3, '2' => 3), 'Int keys';
}

{
    my %h = x => 1;
    my @snap = %h<x>;
    %h<x>++;
    is @snap[0], 1, 'a copy taken before the step keeps the old value';
    is %h<x>, 2, 'the hash holds the new value';
}

{
    my %h;
    my $p = start { 1 };
    await $p;
    %h<t>++ for ^3;
    is %h<t>, 3, 'after a thread has run';
}

{
    my %h;
    for ^2 -> $i {
        my %inner;
        %inner<n>++ for ^($i + 1);
        %h{$i} = %inner<n>;
    }
    is-deeply %h, %('0' => 1, '1' => 2), 'a hash declared in a loop body is fresh each pass';
}
