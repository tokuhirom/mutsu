use Test;

plan 6;

module M { our &g is export = sub { 'g' }; }

# `our &g is export` imported by a block binds `&g` in that block's import set,
# not as an `env` alias the scope exit has to take back out.
{
    import M;
    is g(), 'g', 'a bare call finds the imported code variable';
    is &g.(), 'g', 'the code variable itself is callable';
    my $c = &g;
    is $c(), 'g', 'the code variable can be bound';
    ok MY::.keys.grep('&g'), 'MY:: lists it';
    is (&::('g')).(), 'g', 'a by-name lookup finds it';
}
ok !MY::.keys.grep('&g'), 'the enclosing scope does not list it';
