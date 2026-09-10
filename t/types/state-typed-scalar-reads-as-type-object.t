use Test;

plan 12;

# An undefined typed scalar reads as its TYPE OBJECT, not Nil -- and a `state`
# one must too, on the first call and on every later one. `SetVarType` seeded
# exactly that, but `StateVarInit` then installed the state store's `Nil` over
# the seed, so the seed never survived a single call.

sub typed()   { state Int $u; $u.^name }
sub my_typed(){ my Int $z;    $z.^name }
sub untyped() { state $s;     $s.^name }
sub str_typed(){ state Str $s; $s.^name }

is typed(), 'Int', 'a typed state scalar reads as its type object';
is typed(), 'Int', 'and still does on the second call';
is my_typed(), 'Int', 'the my spelling is unchanged';
is untyped(), 'Any', 'an untyped state scalar is still Any';
is str_typed(), 'Str', 'the constraint decides which type object';

nok (sub { state Int $d; $d.defined })(), 'the type object is undefined';

# An initializer still wins, and persistence is untouched.
sub with_init() { state Int $x = 0; $x }
is with_init(), 0, 'an initializer wins over the seed';
is with_init(), 0, 'and persists';

sub counting() { state Int $p; $p = ($p // 0) + 1; $p }
is counting(), 1, 'a typed state counter starts at 1';
is counting(), 2, 'and accumulates';

# Containers are not scalars and were already right.
is (sub { state Int @a; @a.^name })(), 'Array[Int]', 'a typed state array is unmoved';
is (sub { state Int %h; %h.^name })(), 'Hash[Int]', 'a typed state hash is unmoved';
