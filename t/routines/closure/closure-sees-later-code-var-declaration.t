use Test;

# #18248 (ADR-12529 phase 3): a closure that names a code variable its block
# declares *after* the closure sees that variable, as in rakudo, where a `my`
# lexical is in the scope's pad from the scope's entry. mutsu captured only
# what was declared when the closure was created, and the forward name was
# found by walking the closure's callers -- so a closure returned out of its
# routine called whatever `&f` its caller or the program had instead. Every
# expectation was checked against rakudo 2026.11.

plan 8;

sub f() { 'outer f' }

sub mk() { my &g = -> { f() }; my &f = -> { 'block f' }; &g }
is mk()(), 'block f', 'a returned closure calls the block\'s later `my &f`, not the outer sub';

sub mk-amp() { my &g = -> { &f() }; my &f = -> { 'block f2' }; &g }
is mk-amp()(), 'block f2', '... and so does `&f()`';

sub caller-with-own-f() { my &f = -> { 'caller f' }; mk()() }
is caller-with-own-f(), 'block f', 'a caller\'s own `&f` does not answer the closure';

sub fact-mk() { my &fact = -> $n { $n <= 1 ?? 1 !! $n * fact($n - 1) }; &fact }
is fact-mk()(5), 120, 'a closure in its own declaration\'s initializer recurses through it';

sub two-forward() {
    my &a = { b() ~ '!' };
    my &b = { c() ~ 'b' };
    my &c = { 'c' };
    &a
}
is two-forward()(), 'cb!', 'a chain of forward references';

# Each call of the routine gets its own binding.
sub counter-pair($start) {
    my &get = -> { value() };
    my $n = $start;
    my &value = -> { $n };
    &get
}
my &one = counter-pair(1);
my &two = counter-pair(2);
is "{one()} {two()}", '1 2', 'every activation binds its own forward code variable';

# A loop body's next iteration gets a fresh binding too.
my @closures;
for 1, 2 -> $i {
    my &get = -> { val() };
    my &val = -> { $i * 10 };
    @closures.push(&get);
}
is @closures.map({ $_() }).join(','), '10,20', 'a loop body binds its forward code variable per iteration';

# Calling the closure before the declaration ran finds the variable unset.
sub early() {
    my &g = -> { f() };
    my $r = (try g()) // 'not yet';
    my &f = -> { 'late' };
    $r
}
is early(), 'not yet', 'before its declaration ran the forward variable is unset';
