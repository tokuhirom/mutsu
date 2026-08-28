use v6;
use Test;

# A role mixed onto a TYPE OBJECT (`Any but role Meows {...}`, as opposed to
# an instance) must keep the mixin visible in `.gist`/`.raku`, not just
# `.^name`. Regression: mutsu's `.^name` already correctly composed
# "Any+{Meows}", but `.gist`/`.raku` dropped the mixin entirely and rendered
# the bare base type ("(Any)"/"Any"), because the fast-path method dispatch
# for a role-mixed value delegated straight to the wrapped `inner` Package
# for `gist`/`raku`/`perl` without re-attaching the composed name -- a
# special case already existed for Set/Bag/Mix inners, but not for a plain
# type-object (`Package`) inner.
#
# See roast/6.c/S14-roles/mixin-6c.t tests 48-49 ("method/submethod Bool in
# mixin is used"), whose assertions are `is $m || 42, $m` -- the real
# Test.rakumod's `is` renders both sides via `.gist`, so mutsu's own
# `.^name`-vs-`.gist` disagreement made the two sides of that `is` disagree
# with each other.

plan 8;

my $type_obj = Any but role Meows { method Bool { True } };

is $type_obj.^name, 'Any+{Meows}', 'type object .^name composes the mixin (was already correct)';
is $type_obj.gist, '(Any+{Meows})', 'type object .gist composes the mixin too';
is $type_obj.raku, 'Any+{Meows}', 'type object .raku composes the mixin too';

# The instance case (a role mixed onto a defined value) was already correct
# for .gist/.raku via a different code path -- pin it too so it can't
# silently regress alongside the type-object fix above.
my $instance = Any.new but role Meows2 { method Bool { True } };
is $instance.^name, 'Any+{Meows2}', 'instance .^name composes the mixin';
like $instance.gist, /^ 'Any+{Meows2}.new'/, 'instance .gist composes the mixin';
like $instance.raku, /^ 'Any+{Meows2}.new'/, 'instance .raku composes the mixin';

# A mixin on a builtin numeric/string type object still renders its own
# (unrelated) fast path correctly -- exercise a second, differently-shaped
# base to make sure the fix is general rather than Any-specific.
my $int_type_obj = Int but role Purrs { };
is $int_type_obj.^name, 'Int+{Purrs}', 'Int type object .^name composes the mixin';
is $int_type_obj.gist, '(Int+{Purrs})', 'Int type object .gist composes the mixin';

done-testing;
