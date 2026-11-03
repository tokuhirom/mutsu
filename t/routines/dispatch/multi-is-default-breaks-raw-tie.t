use Test;
# From PDF::COS: `is raw` is no narrowness in rakudo, so `$v is raw` ties with
# `Any:U` and `is default` breaks the tie (a class `COERCE` over a role's).
plan 8;

multi sub s1($v is raw) { "raw" }
multi sub s1(Any:U) is default { "sp" }
is s1(Any), 'sp', 'sub: is default breaks the tie with an is raw candidate';

multi sub a($v is raw) { "raw" }
multi sub a(Any $v) { "any" }
throws-like { a(5) }, X::Multi::Ambiguous, 'is raw does not beat an untyped-equal candidate';

role Base { multi method COERCE($v is raw) { "fallback" } }
class N does Base { multi method COERCE(Any:U) is default { "specific" } }
is N.COERCE(Any), 'specific', 'method: class candidate with is default beats the role fallback';
is N.COERCE(Int), 'specific', 'a type object of another type';
is N.COERCE(5), 'fallback', 'a defined value takes the fallback';

class P {
    multi method C($v is raw) { "fb" }
    multi method C(Any:U) is default { "sp" }
}
is P.C(Any), 'sp', 'same class';

class NC does Base { multi method COERCE(Any:U) is default { self.new } }
my NC() $n = Any;
is $n.WHAT.^name, 'NC', 'coercion type constraint uses the COERCE(Any:U)';

multi sub rw-wins($v is rw) { "rw" }
multi sub rw-wins(Any:U $v) { "anyU" }
my $x = 5;
is rw-wins($x), 'rw', 'is rw still narrows';
