use Test;

plan 9;

# `keyof` of a class composing `Associative[V, K]` is the role's key type.
class CH is Hash does Associative[Cool, DateTime] {}
is CH.new.keyof.raku, 'DateTime', 'instance keyof';
is CH.keyof.raku, 'DateTime', 'type object keyof';
is CH.new.of.raku, 'Cool', 'of is unchanged';

class SUB is CH {}
is SUB.new.keyof.raku, 'DateTime', 'a subclass inherits the composition';

# No key parameter: Str(Any).
class CS is Hash does Associative[Int] {}
is CS.new.keyof.raku, 'Str(Any)', 'Associative[V] keys on Str(Any)';
is CS.keyof.raku, 'Str(Any)', 'Associative[V] type object';

# A parametric role passing its parameter on.
role R[::T] does Associative[Int, T] {}
class BR does R[Str] {}
is BR.keyof.raku, 'Str', 'role passing the key parameter on';

is Hash.keyof.raku, 'Str(Any)', 'Hash.keyof unchanged';
is %().keyof.raku, 'Str(Any)', 'a plain hash keyof unchanged';
