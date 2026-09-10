use Test;

plan 9;

# `$0`/`$1`/... are `$/[0]`/`$/[1]`/...  A match exports each positional
# capture as its own env key, but a directly bound/assigned `$/` has none —
# the digit vars must derive by indexing the current `$/`.

# A non-Match scalar self-indexes: `.[0]` is the value, `.[N>0]` is Nil.
{
    my $/ := 'foobar';
    is $0, 'foobar', '$0 works like $/[0] for a non-Match $/';
    nok $1.defined, '$1 is not defined for a scalar $/';
}

# A bound Array indexes positionally.
{
    my $/ := [10, 20, 30];
    is $0, 10, '$0 indexes a bound Array';
    is $1, 20, '$1 indexes a bound Array';
    is $2, 30, '$2 indexes a bound Array';
    nok $3.defined, '$3 past the end is not defined';
}

# A real match still works (the fast digit-key path).
{
    'abc' ~~ /(.)(.)/;
    is ~$0, 'a', '$0 still works after a match';
    is ~$1, 'b', '$1 still works after a match';
}

# With $/ bound to an undefined value, digit vars are undefined.
{
    my $/ := Any;
    nok $0.defined, '$0 is undefined when $/ is undefined';
}
