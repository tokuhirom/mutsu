use Test;
use lib 't/lib';

plan 7;

# `use strict` must not reject a write to a name that IS declared, but whose
# declaration the by-name store cannot see.

# 1-3: a multi-parameter `for` head declares its parameters. The binds are
# desugared to plain assignments at the top of the loop body, so nothing in the
# emitted store said "this is a declaration".
{
    use strict;

    my %h = a => 1;
    my @seen;
    for %h.kv -> $k, $v { @seen.push("$k=$v") }
    is @seen.join(','), 'a=1', 'scalar multi-param for head binds under strict';

    my @pairs = [1, 2], [3, 4];
    my @firsts;
    for @pairs -> @x, @y { @firsts.push(@x[0] ~ @y[0]) }
    is @firsts.join(','), '13', 'array multi-param for head binds under strict';

    my @flat = 1, 2, 3, 4;
    my $sum = 0;
    for @flat -> $a, $b { $sum += $a * $b }
    is $sum, 14, 'flat list consumed two at a time under strict';
}

# 4-5: the loop parameters are still block-scoped, and an undeclared write
# elsewhere is still rejected.
{
    use strict;
    my $k = 'outer';
    my @a = 1, 2;
    for @a -> $k, $v { }
    is $k, 'outer', 'for parameters do not clobber a same-named outer lexical';
    throws-like '{ use strict; $totally_undeclared = 1 }', X::Undeclared,
        'an undeclared write is still rejected under strict';
}

# 6-7: a module routine writing its own file-scope `my`, reached two frames
# deep, while the *caller* is strict. The lexical lives in the compunit store
# rather than the caller's env, so an env-only declaration test rejected it.
use StrictNestedLexical;
{
    use strict;
    is bump(), 1, 'module routine writes its own file-scope lexical under strict';
    is bump(), 2, 'and the write lands in the module lexical';
}
