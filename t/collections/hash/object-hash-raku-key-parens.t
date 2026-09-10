use v6;
use Test;

# Pair.raku parenthesises a non-concrete (type-object) key and a Pair key;
# an object hash's `.raku` renders each pair the same way, so
# `(my Any %{Mu} = (S) => 7)` round-trips — bare `S => 7` would re-parse
# as the string key "S". mutsu printed the bare key. Expected values
# verified against raku.

plan 6;

my class S { }
my %o{Mu};
%o{S} = 7;
is %o.raku, '(my Any %{Mu} = (S) => 7)', 'a class type-object key is parenthesised';

my role R { }
my %r{Mu};
%r{R} = 3;
is %r.raku, '(my Any %{Mu} = (R) => 3)', 'a role type-object key is parenthesised';

my %p{Mu};
%p{(a => 1)} = 2;
is %p.raku, '(my Any %{Mu} = (:a(1)) => 2)', 'a Pair key is parenthesised';

# Concrete keys stay bare: Str keys keep the colonpair form, numeric and
# Bool keys keep their plain .raku.
my %h{Mu};
%h{"a"} = 1;
%h{42} = 2;
is %h.raku, '(my Any %{Mu} = 42 => 2, :a(1))', 'Str and numeric keys are unchanged';

my %i{Mu};
%i{True} = 5;
is %i.raku, '(my Any %{Mu} = Bool::True => 5)', 'a Bool key stays bare';

# The standalone Pair renderer already parenthesised; pin it too.
is ((S) => 7).raku, '(S) => 7', 'standalone Pair.raku with a type-object key';

done-testing;
