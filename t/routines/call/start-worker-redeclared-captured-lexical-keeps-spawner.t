use Test;

# Red 0.2.7 t/99-transaction-guard: a worker started with `$guard.start(...)`
# re-declares `my $guard` and captures it in a mutated closure. The cell it
# publishes must stay in the worker's lineage; it used to be written through
# to the spawner's same-named `my $guard`, which then changed after `await`.
plan 4;

class G {
    has $.id;
    method start(&b) { my $guard = self; start { $guard!run(&b) } }
    method !run(&b) {
        my $guard = self;
        my &cb = { $guard.id };
        b(:commit(&cb))
    }
}

sub twice(&b) {
    my $guard = G.new(id => 99);
    my $c = { $guard = G.new(id => 98) };
    $c();
    b(:guard($guard))
}

my $guard = G.new(id => 1);
my $p = $guard.start(-> :&commit! { twice(-> :$guard! { $guard.id }); commit() });
is $guard.id, 1, 'spawner binding intact while the worker runs';
is await($p), 1, 'the worker saw its own guard';
is $guard.id, 1, 'spawner binding intact after await';
ok $guard.defined, 'still a G';
