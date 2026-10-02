use Test;

# `%h{$k}++` and friends on a plain hash take a dedicated lane; these pin that
# every shape it accepts or declines still answers what rakudo does.

plan 22;

{
    my %h;
    my $w = 'a';
    is %h{$w}++, 0, 'postfix ++ on an absent key yields 0';
    is %h{$w}++, 1, 'postfix ++ yields the old value';
    is ++%h{$w}, 3, 'prefix ++ yields the new value';
    is %h{$w}--, 3, 'postfix -- yields the old value';
    is --%h{$w}, 1, 'prefix -- yields the new value';
    is --%h<fresh>, -1, 'prefix -- on an absent key counts from 0';
    is-deeply %h, %(a => 1, fresh => -1), 'the hash holds the stepped values';
}

{
    my %counts;
    for ^3 {
        for <x y x z x> -> $w {
            %counts{$w}++;
        }
    }
    is-deeply %counts, %(x => 9, y => 3, z => 3), 'word-frequency loop accumulates';
}

{
    my %h;
    %h{7}++ for ^2;
    is %h<7>, 2, 'an Int subscript keys by its string form';
}

{
    my %h = a => 9223372036854775807;
    %h<a>++;
    is %h<a>, 9223372036854775808, 'overflow past int64 promotes to a big Int';
}

{
    my %h = s => 'a9', n => 1.5e0, r => 1/2;
    %h<s>++;
    %h<n>++;
    %h<r>++;
    is %h<s>, 'b0', 'a Str element takes the string successor';
    is %h<n>, 2.5e0, 'a Num element stays a Num';
    is %h<r>, 3/2, 'a Rat element stays a Rat';
}

{
    my Int %h;
    %h<a>++;
    is %h<a>.WHAT, Int, 'a typed hash increments its element';
    throws-like { my UInt %u; %u<a>-- }, X::TypeCheck::Assignment,
        'the element type still checks the result';
}

{
    my %h is default(10);
    %h<a>++;
    is %h<a>, 11, 'an `is default` element counts from the default';
}

{
    my %h{Any};
    %h{1}++;
    is %h.keys[0].WHAT, Int, 'an object hash keeps the key object';
}

{
    my %h = a => 1;
    my $alias = %h;
    %h<a>++;
    is $alias<a>, 2, 'a second holder of the hash sees the increment';
}

{
    my %seen;
    my &bump = -> $k { %seen{$k}++ };
    bump('q') for ^4;
    is %seen<q>, 4, 'a closure increments the captured hash';
}

{
    my %h;
    sub tally(%t, $k) { %t{$k}++ }
    tally(%h, 'p') for ^3;
    is %h<p>, 3, 'a hash parameter increments the caller hash';
}

{
    my %h = a => 1;
    {
        my %h;
        %h<a>++;
        is %h<a>, 1, 'an inner same-named hash is its own';
    }
    is %h<a>, 1, 'the outer hash is untouched';
}
