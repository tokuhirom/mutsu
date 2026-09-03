use Test;

# `return` can be lexically rebound (`my &return = ...`), so every routine
# return probes the environment for `&return` before raising the return
# signal. That probe is keyed by a pre-interned symbol rather than by name,
# on both the interpreter's `OpCode::Return` and the JIT's `ret` shim -- pin
# that the rebinding still takes effect after the routine has gone hot enough
# to be natively compiled, and that a routine WITHOUT the rebinding is
# unaffected by a sibling one that has it.

plan 4;

sub rebound($n) {
    my &return = sub ($v) { $v + 1000 };
    return $n;
}

sub plain($n) {
    return $n * 2;
}

is rebound(1), 1001, 'a rebound &return runs instead of the built-in return';
is plain(1), 2, 'a routine without the rebinding returns normally';

my $hot = 0;
my $cold = 0;
for ^20000 {
    $hot += rebound(1);
    $cold += plain(1);
}
is $hot, 20020000, 'the rebinding still applies once the routine is hot';
is $cold, 40000, 'a plain return is unaffected by the sibling rebinding';
