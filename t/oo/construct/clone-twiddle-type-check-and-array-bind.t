use Test;

# From Selkie (t/100-store-schema.rakutest): `.clone(attr => value)` assigns the
# attribute, so a typed one refuses a value of the wrong type, and an `@`
# attribute is bound to an itemized Array as it is.
plan 7;

class C { has Int $.count = 0; has @.tasks; has %.prefs; }
my $c = C.new(count => 1, tasks => <a b>, prefs => { a => 1 });

throws-like { $c.clone(count => 'not-an-int') }, X::TypeCheck::Assignment,
    'a clone twiddle is type-checked';
is $c.clone(count => 7).count, 7, 'a well-typed twiddle is applied';

my $empty = [];
is-deeply $c.clone(tasks => $empty).tasks, [], 'an itemized empty Array resets the @ attribute';
my $two = [1, 2];
is-deeply $c.clone(tasks => $two).tasks, [1, 2], 'an itemized Array replaces the @ attribute';
is-deeply C.new(tasks => $two).tasks, [[1, 2],], 'new still wraps the item';
my $h = {};
is-deeply $c.clone(prefs => $h).prefs, {}, 'an itemized Hash resets the % attribute';
is $c.tasks.elems, 2, 'the original is untouched';
