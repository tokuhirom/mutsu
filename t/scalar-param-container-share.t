use Test;

# Slice 2d: passing an array/hash *variable* to a readonly scalar `$` param
# binds the same mutable container (Raku reference semantics). `$n.push`,
# `$n[0]=`, and `my @a := @$n` all mutate the caller's container; only `$n = …`
# rebinding is forbidden. Previously the deref-bind form lost the mutation and
# `$n.push` propagated only by statement-order luck.

plan 19;

# --- canonical deref-bind (the bug) ---
{
    sub f($n) { my @a := @$n; @a.push(99) }
    my @z = (1, 2);
    f(@z);
    is @z.gist, '[1 2 99]', 'deref-bind push propagates to caller';
    is @z.elems, 3, 'deref-bind push changes elems';
}

# --- direct mutation, robust regardless of statement order ---
{
    sub g($n) { my $x = $n.elems; $n.push(99); $x }
    my @z = (10, 20);
    g(@z);
    is @z.gist, '[10 20 99]', '$n.push propagates even after a read';
}
{
    sub h($n) { $n[0] = 50 }
    my @z = (1, 2);
    h(@z);
    is @z.gist, '[50 2]', '$n[0]= propagates';
}

# --- hash param ---
{
    sub hp($n) { $n<b> = 2 }
    my %h = (a => 1);
    hp(%h);
    is %h.sort.gist, '(a => 1 b => 2)', 'hash $-param element assign propagates';
}
{
    sub hpush($n) { my %x := %$n; %x<c> = 9 }
    my %h = (a => 1);
    hpush(%h);
    is %h.sort.gist, '(a => 1 c => 9)', 'hash deref-bind element assign propagates';
}

# --- readonly: `$n = …` rebinding forbidden ---
{
    sub ro($n) { $n = 5 }
    my @z = (1, 2);
    dies-ok { ro(@z) }, '$n = … on a scalar container param dies (readonly)';
    is @z.gist, '[1 2]', 'caller unchanged after readonly violation';
}

# --- no source variable: literal arg does not error and does not share ---
{
    sub lit($n) { $n.push(0); $n.elems }
    is lit([1, 2]), 3, 'array literal arg mutates locally without error';
}

# --- value semantics preserved (itemization / type / interpolation) ---
{
    my @a = (1, 2, 3);
    sub elems($n) { $n.elems }
    is elems(@a), 3, '.elems through scalar param';
    sub what($n) { $n.WHAT.^name }
    is what(@a), 'Array', '.WHAT through scalar param is Array';
    sub interp($n) { "v: $n" }
    is interp(@a), 'v: 1 2 3', 'interpolation through scalar param';
    is @a.gist, '[1 2 3]', 'pure read does not mutate caller';
}

# --- return / store does not leak a ContainerRef ---
{
    sub ret($n) { $n }
    my @a = (1, 2, 3);
    my $r = ret(@a);
    is $r.WHAT.^name, 'Array', 'returned scalar param is a plain Array';
    is $r.gist, '[1 2 3]', 'returned value is correct';
}

# --- nested forwarding: sub -> sub ---
{
    sub inner($m) { $m.push(7) }
    sub outer($n) { inner($n) }
    my @q = (1,);
    outer(@q);
    is @q.gist, '[1 7]', 'mutation propagates through a forwarded scalar param';
}

# --- typed param ---
{
    sub typed(Array $n) { $n.push(9) }
    my @b = (1, 2);
    typed(@b);
    is @b.gist, '[1 2 9]', 'typed Array scalar param shares';
}

# --- multiple params, mixed scalar/container ---
{
    sub mix($x, $y) { $x.push(9); $y + 1 }
    my @p = (1, 2);
    is mix(@p, 5), 6, 'scalar non-container param still works alongside';
    is @p.gist, '[1 2 9]', 'container param shares with mixed signature';
}
