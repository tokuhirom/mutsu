use Test;

plan 2;

# A closure created before a routine-nested `my sub` is registered, and that
# calls the sub, must see the same variable the sub reads, even when the
# routine's caller has a same-named variable.
sub inner() {
    my $value;
    my $n = 0;
    my &leaf = { $value = ++$n };
    my &branch = { my @a; @a.push(el(1)) for ^2; $value = @a };
    my sub el($k) {
        $k ?? leaf() !! branch();
        $value
    }
    el(0);
}

sub outer() { my $value = inner(); $value }
is outer().join(','), '1,2', 'caller with a same-named variable';

sub outer2() { my $v = inner(); $v }
is outer2().join(','), '1,2', 'caller without one';
