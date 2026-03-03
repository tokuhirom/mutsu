use Test;

plan 31;

sub j(*@i) {
    @i.map({ $_ ?? '1' !! '0' }).join(' ');
}

# Basic Parameter introspection
{
    sub a($x, Int $y?, :$z) { };
    ok &a.signature.params ~~ Positional, '.params does Positional';
    my @l = &a.signature.params;
    is +@l, 3, 'we have three of them';
    is ~(@l>>.name), '$x $y $z', 'can get the names with sigils';
    ok @l[0].type === Any, 'Could get first type (Any)';
    ok @l[1].type === Int, 'Could get second type (Int)';

    is j(@l>>.readonly), '1 1 1', 'they are all read-only';
    is j(@l>>.rw),       '0 0 0', '... none rw';
    is j(@l>>.copy),     '0 0 0', '... none copy';
    is j(@l>>.raw),      '0 0 0', '... none raw';
    is j(@l>>.slurpy),   '0 0 0', '... none slurpy';
    is j(@l>>.optional), '0 1 1', '... some optional';
    is j(@l>>.invocant), '0 0 0', '... none invocant';
    is j(@l>>.named),    '0 0 1', '... one named';
}

# Trait-modified params
{
    sub b(:x($a) is rw, :$y is raw, :$z is copy) { };
    my @l = &b.signature.params;
    is j(@l>>.readonly), '0 0 0', '(second sig) none are read-only';
    is j(@l>>.rw),       '1 0 0', '... one rw';
    is j(@l>>.raw),      '0 1 0', '... one raw';
    is j(@l>>.copy),     '0 0 1', '... one copy';
    is j(@l>>.named),    '1 1 1', '... all named';

    is ~@l[0].named_names, 'x',   'named_names work';
    is ~@l[0].name,      '$a',    '.name works for renamed params';
}

# Slurpy params
{
    sub d(*@pos, *%named) { };
    my @l = &d.signature.params;
    is j(@l>>.named),    '0 1', '.named for slurpies (hash is named)';
    is j(@l>>.slurpy),   '1 1', '.slurpy';
    is ~(@l>>.name),     '@pos %named', '.name for slurpies';
}

# Multi-level named aliases
{
    sub d(:x(:y(:z($a)))) { };
    is ~&d.signature.params.[0].named_names.sort, 'x y z', 'multi named_names';
    is ~&d.signature.params.[0].name, '$a', '... and .name still works';
}

# Capture param introspection
{
    sub xyz(|c) {};
    is &xyz.signature.params[0].name,       'c',    '.name of |c is "c"';
    is &xyz.signature.params[0].positional, False,  '.positional on Capture param is False';
    is &xyz.signature.params[0].capture,    True,   '.capture on Capture param is True';
    is &xyz.signature.params[0].named,      False,  '.named on Capture param is False';
}

# Positional attribute
{
    sub f($x, :$y) { };
    my @l = &f.signature.params;
    is @l[0].positional, True,  '$x is positional';
    is @l[1].positional, False, ':$y is not positional';
}
