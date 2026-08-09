use Test;

plan 45;

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
    is ~&d.signature.params.[0].named_names, 'z y x',
        'alias chain is reported innermost-first (rakudo order)';
}

# named_names on plain named params (no alias sub-signature)
{
    sub e(:$x, *%h) { };
    is ~&e.signature.params[0].named_names, 'x', 'plain :$x has its own name as named_names';
    is +&e.signature.params[1].named_names, 0, 'slurpy *%h has no named_names';

    my $blk = -> :$min-price { };
    my $p = $blk.signature.params.grep(*.named).head;
    is ~$p.named_names, 'min-price', 'pointy-block plain named param has named_names';

    sub f(:y($a)) { };
    is ~&f.signature.params[0].named_names, 'y', 'single alias reports the external name only';
}

# Subset types are nominalized: .type reports the base nominal type and the
# subset lands in .constraints (Cro's route compiler relies on both).
{
    sub g(UInt :$page) { };
    my $p = &g.signature.params[0];
    ok $p.type =:= Int, 'UInt parameter .type is the nominal base Int';
    is $p.constraints.raku, 'all(UInt)', '... and .constraints carries the subset';

    sub h(Int :$n) { };
    is &h.signature.params[0].constraints.raku, 'all()',
        'a plain nominal type leaves .constraints empty';
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

# constraint_list: the underlying List behind the .constraints junction
# (Cro's route compiler uses both: `$p.constraint_list == 1 && ...`).
{
    sub with-literal("x") { }
    is &with-literal.signature.params[0].constraint_list.raku, '("x",)',
        'constraint_list reflects a literal-value constraint';

    sub with-where($x where * > 0) { }
    is +&with-where.signature.params[0].constraint_list, 1,
        'constraint_list reflects a where-clause constraint';

    sub no-constraint($x) { }
    is +&no-constraint.signature.params[0].constraint_list, 0,
        'constraint_list is empty for an unconstrained parameter';
}

# usage-name: the variable name minus sigil and twigil
{
    sub named-usage($x, :$y) { }
    is &named-usage.signature.params[0].usage-name, 'x',
        'usage-name strips the sigil from a positional parameter';
}

# default: the undefined Code type object when absent, not a missing method
{
    sub no-default($x) { }
    is &no-default.signature.params[0].default.raku, 'Code',
        'default is the undefined Code type object when absent';
    nok &no-default.signature.params[0].default.defined,
        'default is undefined when absent';
}
