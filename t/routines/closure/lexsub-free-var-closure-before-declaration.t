use Test;

plan 3;

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

# Two nested subs hoisted above the same later `my` declaration share its
# container (Template::Mustache's `render`).
sub two-subs() {
    my @seen = ();
    sub add($x) { @seen.push: $x }
    add(1);
    return show();
    sub show() { @seen.join(',') }
}
is two-subs(), '1', 'both hoisted subs see the declared variable';
