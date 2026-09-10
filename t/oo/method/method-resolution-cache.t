use Test;
use MONKEY-TYPING;

# Guards the non-multi method-resolution cache on the compiled-mut dispatch path
# (resolve_method_cached): repeated dispatch must stay correct, inheritance must
# resolve to the right candidate, and runtime method changes must invalidate the
# cache (it is cleared together with last_method_resolve at every registry
# mutation site).

plan 7;

# 1. Repeated dispatch of the same non-multi method (cache hit path) stays correct.
class Counter {
    has $.base;
    method scaled($k) { $!base * $k }
}
my $c = Counter.new(base => 3);
my $ok = True;
$ok &&= ($c.scaled($_) == 3 * $_) for 1..50;
ok $ok, 'repeated non-multi dispatch returns correct results across many calls';

# 2. Inheritance: the cache must key on the *receiver* class, not leak a parent's
#    resolution to a sibling.
class Animal { method sound { 'generic' } }
class Dog is Animal { method sound { 'woof' } }
class Cat is Animal { method sound { 'meow' } }
my @sounds = (Dog.new, Cat.new, Animal.new, Dog.new).map(*.sound);
is @sounds.join(','), 'woof,meow,generic,woof', 'per-class resolution not cross-contaminated';

# 3. Same method name on two unrelated classes resolves independently.
class Greeter { method hello { 'hi' } }
class Yeller  { method hello { 'HI' } }
is (Greeter.hello, Yeller.hello, Greeter.hello).join('|'), 'hi|HI|hi',
    'same method name on unrelated classes resolves per class';

# 4. Cache invalidation: augmenting a class with a new method must be seen even
#    after the class has already been dispatched against (cache cleared on the
#    registry mutation).
class Widget { method kind { 'base' } }
is Widget.kind, 'base', 'pre-augment resolution';
augment class Widget { method extra { 'added' } }
is Widget.extra, 'added', 'augmented method is visible (cache invalidated)';

# 5. Warm the cache for an existing method, then augment in a *new* method; both
#    the previously-cached method and the freshly-added one must resolve.
class Box { method size { 1 } }
is Box.size, 1, 'pre-augment method resolves (warms the cache)';
augment class Box { method colour { 'red' } }
is (Box.size, Box.colour).join('/'), '1/red',
    'cached method still works and newly-augmented method resolves';
