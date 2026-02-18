use Test;

plan 12;

# Bare sigil declarations
lives-ok { my $ }, 'bare sigil $';
lives-ok { my @ }, 'bare sigil @';
lives-ok { my % }, 'bare sigil %';

# Initialized bare sigils
is (my $ = "foo"), "foo", 'initialized bare sigil scalar';
ok (my @ = 1, 2, 3), 'initialized bare sigil array';

# State scalar
{
    sub f { ++state $; }
    is (f, f, f), (1, 2, 3), 'state bare sigil scalar retains state';
}

# State scalar with init
{
    sub g { ++state $ = 3; }
    is (g, g, g), (4, 5, 6), 'state bare sigil scalar initialized once';
}

# State array
{
    sub d { state $i = 0; (state @).push( $i++ ) }
    d;
    is +d(), 2, 'state bare sigil array retains state';
    is d()[2], 2, 'state bare sigil array can grow';
}

# Named state variable
{
    sub h { state $x = 10; $x++ }
    is h(), 10, 'named state scalar first call';
    is h(), 11, 'named state scalar second call';
    is h(), 12, 'named state scalar third call';
}
