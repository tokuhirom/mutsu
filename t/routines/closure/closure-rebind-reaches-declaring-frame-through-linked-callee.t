use Test;

# A closure that rebinds an outer `@`-variable, called by a routine declared at
# program scope, must reach the frame that declared the variable. The callee's
# frame skips its callers for lexical lookups (ADR-12529), but the writeback
# carries a value across frames and must still find it (#12478). Checked
# against rakudo.

plan 4;

sub run-it(&code) { code(); True }
sub run-block(&b) { b() }

run-block({
    my @y;
    run-it({ @y := [3] });
    is @y.raku, '[3]', 'a rebind in a block passed to a routine reaches the block';
});

sub in-routine {
    my @r;
    run-it({ @r := [1, 2] });
    @r.raku
}
is in-routine(), '[1, 2]', 'a rebind reaches the routine that declared the array';

run-block({
    my @a[2;2];
    my @result;
    run-it({ @result := @a.Array });
    is-deeply @result, [Any xx 4], 'a rebind to a shaped array\'s .Array';
});

run-block({
    my $s;
    run-it({ $s := 5 });
    is $s, 5, 'a scalar rebind reaches the block';
});

done-testing;
