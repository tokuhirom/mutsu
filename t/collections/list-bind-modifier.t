use Test;

# `my ($a, @b) := RHS if COND` binds the elements (it is not a list
# assignment) and declares the names even when COND is false.
# Found via Red's `create` (`my ($where, @wb) := ... if $ast.?filter`),
# distribution RedX::HashedPassword.

{
    my ($a, @b) := (1, [2, 3]) if 1;
    is-deeply @b, [2, 3], 'array element is bound, not wrapped in a one-item array';
    is $a, 1, 'scalar element bound';
}

{
    my ($c, @d) := (1, [2, 3]) if 0;
    is-deeply @d, [], 'false condition: array declared and empty';
    ok !$c.defined, 'false condition: scalar declared and undefined';
}

{
    my ($p, $q) := (3, 4) unless 1;
    ok !$p.defined, 'unless with true condition does not bind';
}

{
    my @x = 5, 6;
    my (@y,) := (@x,) unless 0;
    @y.push(7);
    is-deeply @x, [5, 6, 7], 'bound array aliases the source';
}

{
    sub t() { "k" => [] }
    my @bind;
    my ($w, @wb) := do given t() { .key, .value } if 1;
    @bind.push: |@wb;
    is-deeply @bind, [], 'empty bound array pushes nothing';
    is $w, 'k', 'key bound';
}

{
    my ($e, @f) = (1, [2, 3]) if 1;
    is-deeply @f, [[2, 3],], 'list assignment form still slurps the remainder';
}

done-testing;
