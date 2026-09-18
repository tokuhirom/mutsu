use v6;
use Test;

# A `multi` family whose candidates are distinguished by `subset` constraints is
# value-dependent as a family, so its winner used to be re-resolved from scratch
# on every call (#8696). The resolution is now cached per argument-type key
# whenever every value-dependent candidate is ruled out by its DECLARED base
# type for those argument types -- which is decidable without running a single
# predicate. These tests pin the cases where that narrowing must NOT change the
# answer.

plan 30;

# --- The base type matches but the predicate rejects -------------------------
# `Small` is `of Int`, so an Int argument survives base-type narrowing and the
# predicate still has to run. A cached winner here would be flatly wrong.

subset Small of Int where * < 10;

multi swin(Small $x) { "small" }
multi swin(Int $x)   { "int" }
multi swin(Str $x)   { "str" }

is swin(3), "small", "subset predicate accepts a matching value";
is swin(99), "int", "subset predicate rejects, the wider Int candidate wins";
is swin(4), "small", "and the accepting case still works after a rejection";
is swin(50), "int", "and the rejecting case still works after an acceptance";
is swin("x"), "str", "a Str argument reaches the Str candidate";

# --- Two subsets over the same base, different predicates --------------------

subset Evenish of Int where * %% 2;
subset Oddish  of Int where * % 2 == 1;

multi parity(Evenish $x) { "even" }
multi parity(Oddish $x)  { "odd" }
multi parity(Str $x)     { "str" }

is parity(4), "even", "first subset over Int";
is parity(7), "odd", "second subset over the same base";
is parity(8), "even", "and back again (no cached winner leaks across values)";
is parity("s"), "str", "a Str argument skips both Int subsets";

# --- A subset over a user class in an inheritance chain ----------------------

class Animal {}
class Dog is Animal {}
class Rock {}

subset Loud of Animal where { True };

multi speak(Loud $a) { "loud" }
multi speak(Rock $r) { "rock" }

is speak(Dog.new), "loud", "a subclass instance satisfies a subset of its base";
is speak(Animal.new), "loud", "the base class itself satisfies it too";
is speak(Rock.new), "rock", "an unrelated class is narrowed away from the subset";
is speak(Dog.new), "loud", "the subclass answer is stable on a repeat call";

# --- A subset chain (subset of a subset) -------------------------------------

subset Mid of Int where * > 0;
subset Top of Mid where * < 100;

multi chained(Top $x) { "top" }
multi chained(Str $x) { "str" }

is chained(5), "top", "a subset of a subset resolves through the chain";
is chained("s"), "str", "the whole chain is narrowed away for a Str argument";

# --- Ambiguity is still raised -----------------------------------------------
# The three-argument subset candidate makes this family value-dependent, and a
# two-argument call narrows it away by arity -- which is exactly when the
# resolution becomes cacheable. A genuinely tied dispatch among what is left
# must still raise, on every call, rather than be answered from a cache.

subset Trio of Int where { True };

multi amb(Int $x, Any $y)             { "a" }
multi amb(Any $x, Int $y)             { "b" }
multi amb(Trio $x, Trio $y, Trio $z)  { "c" }

dies-ok { amb(1, 2) }, "a genuinely tied dispatch still dies ambiguous";
dies-ok { amb(1, 2) }, "and dies again on the second call, not from a cache";
is amb(1, "s"), "a", "an untied call in the same family still resolves";
is amb(1, 2, 3), "c", "the narrowed-away candidate is still reachable at its own arity";

# --- A later parameter's exclusion must not skip an earlier side effect ------
# `sideline`'s first parameter carries a `where` whose side effect the uncached
# path performs before the second parameter's type check rejects the candidate.
# The narrowing is only allowed to exclude a candidate when the excluding
# parameter sits at or before the first one that can run user code, so this
# candidate must keep being tried on every call.

my $ran = 0;
multi sideline(Int $a where { $ran++; True }, Int $b) { "int-int" }
multi sideline(Int $a, Str $b) { "int-str" }

is sideline(1, "x"), "int-str", "the later parameter's type rejects the guarded candidate";
is sideline(2, "y"), "int-str", "and again on a repeat call";
is sideline(1, 2), "int-int", "the where-guarded candidate still wins when it binds";
ok $ran > 0, "the where clause did run for the call it guards";

# --- A wider argument type reaches a different winner ------------------------
# Same family, three different argument-type keys: each must get its own
# answer, not the first one that was cached.

subset Tiny of Int where * < 3;

multi wide(Tiny $x) { "tiny" }
multi wide(Int $x)  { "int" }
multi wide(Str $x)  { "str" }
multi wide(Rat $x)  { "rat" }

is wide("a"), "str", "Str argument";
is wide(1), "tiny", "Int argument inside the subset";
is wide(1/2), "rat", "Rat argument";
is wide(9), "int", "Int argument outside the subset";
is wide("a"), "str", "back to the Str argument";

# --- Definedness still discriminates -----------------------------------------

subset DefStr of Str where { True };
multi smiley(DefStr:D $x) { "defined" }
multi smiley(Str:U $x)    { "undefined" }

is smiley("x"), "defined", "a defined Str reaches the subset candidate";
is smiley(Str), "undefined", "a type object reaches the :U candidate";

done-testing;
