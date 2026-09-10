use Test;
plan 16;

# full-barrier
lives-ok { full-barrier() }, 'full-barrier lives';

# atomic-fetch and atomic-assign
{
    my Int $x = 42;
    is atomic-fetch($x), 42, 'atomic-fetch returns current value';
    is atomic-assign($x, 100), 100, 'atomic-assign returns new value';
    is atomic-fetch($x), 100, 'atomic-fetch after assign returns updated value';
    is $x, 100, 'variable updated after atomic-assign';
}

# atomic-fetch-inc / atomic-fetch-dec
{
    my atomicint $x = 10;
    is atomic-fetch-inc($x), 10, 'atomic-fetch-inc returns old value';
    is atomic-fetch-inc($x), 11, 'atomic-fetch-inc incremented';
    is atomic-fetch-dec($x), 12, 'atomic-fetch-dec returns old value';
    is atomic-fetch-dec($x), 11, 'atomic-fetch-dec decremented';
}

# atomic-inc-fetch / atomic-dec-fetch
{
    my atomicint $x = 10;
    is atomic-inc-fetch($x), 11, 'atomic-inc-fetch returns new value';
    is atomic-dec-fetch($x), 10, 'atomic-dec-fetch returns new value';
}

# atomic-fetch-add / atomic-add-fetch
{
    my atomicint $x = 10;
    is atomic-fetch-add($x, 5), 10, 'atomic-fetch-add returns old value';
    is atomic-fetch($x), 15, 'value updated after atomic-fetch-add';
    is atomic-add-fetch($x, 5), 20, 'atomic-add-fetch returns new value';
}

# atomicint is recognized as a type
ok atomicint.Range.defined, 'atomicint.Range is defined';

# type check on atomic-assign
dies-ok { my Int $y = 42; atomic-assign($y, 'not-an-int') }, 'atomic-assign type check dies';
