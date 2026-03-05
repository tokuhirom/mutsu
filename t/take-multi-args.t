use Test;

plan 5;

# take with a single argument
{
    my @a = eager gather { take 1 };
    is @a.elems, 1, 'take with single arg produces 1 element';
    is @a[0], 1, 'take with single arg: correct value';
}

# take with multiple comma-separated arguments (creates a list)
{
    my @a = eager gather { take 1, 2, 3 };
    is @a.elems, 1, 'take 1,2,3 produces 1 element (a list)';
    is @a[0].elems, 3, 'the taken list has 3 elements';
}

# take with multiple separate takes
{
    my @a = eager gather { take 1; take 2; take 3 };
    is @a.elems, 3, 'three separate takes produce 3 elements';
}
