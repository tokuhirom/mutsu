use Test;

# `.^find_method` on a class with a `FALLBACK` answers a missing name with a
# dispatcher that calls `FALLBACK` (#16888, EEL 0.3's `Event`); without one,
# or with `:no_fallback`, it answers `Mu`.

plan 8;

class Plain { method here { 1 } }
class Catcher {
    method here { 'own' }
    method FALLBACK($name, *@args) { "fallback $name: @args[]" }
}
class Child is Catcher { }

ok Plain.^find_method('nope') =:= Mu, 'no FALLBACK: a missing name is Mu';

my $m = Catcher.^find_method('nope');
ok $m.defined, 'a FALLBACK class answers a missing name';
ok $m ~~ Callable, 'with a callable';
is $m(Catcher.new, 1, 2), 'fallback nope: 1 2', 'that calls FALLBACK with the name';
is Catcher.^find_method('here')(Catcher.new), 'own', 'a declared method is still itself';
ok Catcher.^find_method('nope', :no_fallback) =:= Mu, ':no_fallback skips the fallback';
ok Child.^find_method('nope').defined, 'an inherited FALLBACK counts';

sub takes-any($method) { $method.defined }
ok takes-any(Child.^find_method('missing')), 'the answer binds to an Any parameter';
