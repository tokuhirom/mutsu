use Test;
# From the P5pack distribution: a sub declaring its own nested sub, called
# from inside a closure (Lock.protect), must find the nested sub's body.
plan 2;

my $lock := Lock.new;
my sub outer($template) {
    sub parse($t) {
        sub is-ws($s) { $s eq " " }
        is-ws($t)
    }
    $lock.protect: { parse($template) }
}
is outer(" "), True, 'nested sub of a sub called from a closure';
is outer("a"), False, 'and again';
