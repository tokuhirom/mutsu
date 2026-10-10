use Test;

# A closure's call through a captured `&`-parameter calls the binding it
# captured, read by upvalue index, whatever `&name` the calling frame has
# (ADR-12529 §2.1).

plan 7;

sub apply(&f, &p) { -> @x { &p(@x).map({ ($_[0], &f($_[1])) }) } }
is-deeply apply({ $_ * 2 }, -> @y { ((1, 2),) })([1]).List, ((1, 4),),
    'a closure calls the captured &p and a nested block the captured &f';

sub call-later(&cb) { -> $x { &cb($x) ~ '!' } }
my $c = call-later(-> $v { "cb:$v" });
sub caller-with-own-cb() { my &cb = -> $v { "wrong:$v" }; $c(1) }
is caller-with-own-cb(), 'cb:1!', "a caller's own &cb does not shadow the captured one";
is $c(2), 'cb:2!', 'the captured binding survives its routine';

sub bare-call(&cb) { -> $x { cb($x) } }
is bare-call(-> $v { $v * 3 })(5), 15, 'a bare call of the captured &cb';

sub with-copy(&cb is copy) { my $k = -> { &cb() }; &cb = -> { 'rebound' }; $k }
is with-copy(-> { 'orig' })(), 'rebound', 'an `is copy` parameter is still rebindable';

sub with-my() { my &f = -> { 'one' }; my $k = -> { &f() }; &f = -> { 'two' }; $k }
is with-my()(), 'two', 'a `my &f` reassigned after the closure is seen';

my @made = (1..3).map(-> $n { call-later(-> $v { "$n/$v" }) });
is @made.map({ $_('x') }).join(','), '1/x!,2/x!,3/x!', 'each closure keeps its own capture';
