use v6;
use Test;

plan 5;

my $x = 42;
{
    my $name = '$x';

    is OUTER::MY::{'$x'}, 42,
        'OUTER::MY:: resolves a symbol from the immediate outer scope';
    is OUTER::MY::{$name}, 42,
        'OUTER::MY:: supports a dynamically selected symbol';
    is OUTER::MY::<<$name>>, 42,
        'OUTER::MY:: supports an interpolating angle subscript';
    ok OUTER::MY::<<$name>>:exists,
        'OUTER::MY:: reports the dynamically selected symbol as existing';
    nok MY::<$x>:exists,
        'MY:: does not see a lexical declared by an enclosing scope';
}

done-testing;
