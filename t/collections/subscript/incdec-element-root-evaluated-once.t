use Test;

# `++`/`--` on an element whose chain is rooted in something other than a
# variable evaluates that root once: a call is not repeated, and a `constant`
# declaration is not redeclared (#10582). The write still lands in the root's
# own container.

plan 16;

{
    my @a = 1, 2;
    my $calls = 0;
    sub f { $calls++; @a }
    f()[0]++;
    is-deeply @a, [2, 2], 'f()[0]++ writes into the returned array';
    is $calls, 1, 'f()[0]++ calls f once';
    ++f()[1];
    is-deeply @a, [2, 3], '++f()[1] writes into the returned array';
    is $calls, 2, '++f()[1] calls f once';
    f()[1]--;
    --f()[0];
    is-deeply @a, [1, 2], 'postfix and prefix -- through a call';
    is $calls, 4, '-- calls f once each';
}

{
    my @a = [1, 2], [3];
    my $calls = 0;
    sub g { $calls++; @a }
    g()[0][1]++;
    is-deeply @a, [[1, 3], [3]], 'multi-level chain rooted in a call';
    is $calls, 1, 'multi-level: the call runs once';
}

{
    (constant w = [1, 2, 3])[0]++;
    is-deeply w, [2, 2, 3], '(constant w = [...])[0]++';
    ++(constant v = [1, 2, 3])[1];
    is-deeply v, [1, 3, 3], '++(constant v = [...])[1]';
    is (constant z = [5])[0]++, 5, 'postfix ++ on a constant element yields the old value';
    is ++(constant y = [5])[0], 6, 'prefix ++ on a constant element yields the new value';
}

{
    class C { has %.h is rw; has @.l; method list { @!l } }
    my $o = C.new(l => [1, 2]);
    $o.h<k>++;
    is-deeply $o.h, { k => 1 }, 'an rw hash attribute element autovivifies';
    $o.list[1]--;
    is-deeply $o.l, [1, 1], 'a method-returned array element is written in place';
}

{
    my %h;
    (%h)<a><b>++;
    is-deeply %h, { a => { b => 1 } }, 'parentheses around a variable root: (%h)<a><b>++ autovivifies';
    my @a = [1, 2],;
    ++(@a)[0][1];
    is-deeply @a, [[1, 3],], '++(@a)[0][1]';
}
