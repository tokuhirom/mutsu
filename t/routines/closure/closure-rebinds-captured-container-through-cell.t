use Test;

# A closure that rebinds (`:=`) an `@`/`%` of the frame that created it, called
# from another routine, reaches that frame through a shared binding cell: the
# frame, sibling closures made before or after it, and later rebinds all see
# the new binding, while a name bound to the old container keeps it. No
# frame's return merge carries the rebind (ADR-12529 §3, slice 25). Checked
# against rakudo.

plan 9;

sub run-it(&code) { code() }

{
    my @y = 1, 2;
    my @w := @y;
    my &peek = { @y.elems };
    run-it({ @y := [3, 4, 5] });
    is @y.raku, '[3, 4, 5]', 'the creating block sees the rebind';
    is @w.raku, '[1, 2]', 'a name bound to the old array keeps it';
    is peek(), 3, 'a sibling closure made before the rebind sees it';
    @y.push: 6;
    is peek(), 4, 'and the block\'s later mutation of the new array';
    @y = 7;
    is @y.raku, '[7]', 'an assignment after the rebind writes the new array';
    is @w.raku, '[1, 2]', 'and not the old one';
}

{
    my %h = a => 1;
    run-it({ %h := { b => 2 } });
    is %h.raku, '{:b(2)}', 'a hash rebind reaches the creating block';
}

{
    my @z = 1;
    run-it({ @z := [] }) for ^2;
    is @z.elems, 0, 'repeated rebinds through fresh closures';
}

sub in-routine {
    my @r;
    run-it({ @r := [1, 2] });
    @r.raku
}
is in-routine(), '[1, 2]', 'a rebind reaches the routine that declared the array';
