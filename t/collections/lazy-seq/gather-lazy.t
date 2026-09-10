use Test;

plan 8;

# Basic laziness: gather body only runs when elements are accessed
{
    my $ran = False;
    my @list := gather { $ran = True; take 1; take 2; take 3 };
    ok !$ran, "gather body has not run before access";
    is @list[0], 1, "first element is correct after lazy access";
}

# Coroutine-style laziness: side effects happen incrementally
{
    my $count = 0;
    my @list := gather {
        for 1 .. 10 -> $a {
            take $a;
            $count++;
        }
    }.List;
    my $result = @list[2];
    is $count, 2, "gather is lazy - only 2 increments after accessing 3rd element";
    is $result, 3, "3rd element has correct value";
}

# Assignment (not binding) forces eagerly
{
    my $count = 0;
    my @list = gather {
        for 1 .. 5 -> $a {
            take $a;
            $count++;
        }
    };
    is $count, 5, "assignment forces all elements eagerly";
}

# Scalar binding preserves laziness
{
    my $x = 0;
    my $g = gather {
        $x += 1;
        take $x;
    };
    is $x, 0, 'gather is lazy with scalar binding';
}

# Full forcing via stringification
{
    my @list = gather { take 1; take 2; take 3 };
    is ~@list, "1 2 3", "gather forced via stringification";
}

# Nested gathers work
{
    my @outer = gather {
        for 1..3 -> $i {
            my @inner = gather { take $_ for 1..3 };
            take "$i:" ~ @inner.join(',');
        }
    };
    is ~@outer, "1:1,2,3 2:1,2,3 3:1,2,3", "nested gather works";
}
