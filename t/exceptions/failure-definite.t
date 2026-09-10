use v6;
use Test;

# `.DEFINITE` is the concreteness primitive: True iff the object is a concrete
# instance (not a type object). It is distinct from `.defined`, which Failure
# overrides to return False. A `Failure.new(...)` is a concrete instance, so its
# `.DEFINITE` is True even though its `.defined` is False.
# From raku-doc Language/signatures.rakudoc (doc-diff finding [13]).

plan 8;

my $type-obj = Failure;              # type object
my $instance = Failure.new("foo");   # concrete instance

is $type-obj.DEFINITE, False, 'Failure type object: DEFINITE is False';
is $instance.DEFINITE, True,  'Failure instance: DEFINITE is True (concrete)';
is $type-obj.defined,  False, 'Failure type object: defined is False';
is $instance.defined,  False, 'Failure instance: defined is False (override)';

# The fix does not disturb DEFINITE for ordinary values / type objects.
is 42.DEFINITE,  True,  'Int value: DEFINITE True';
is Int.DEFINITE, False, 'Int type object: DEFINITE False';

class C { }
is C.new.DEFINITE, True,  'user instance: DEFINITE True';
is C.DEFINITE,     False, 'user type object: DEFINITE False';
