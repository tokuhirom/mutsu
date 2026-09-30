use Test;

plan 4;

class BoundaryPlain {
    method inner() { callsame }
}
multi sub boundary-outer(Int $x) {
    "int(" ~ (BoundaryPlain.new.inner // 'Nil') ~ ")"
}
multi sub boundary-outer(Any $x) { "any" }
is boundary-outer(1), 'int(Nil)', 'a plain method does not defer to an enclosing multi sub';

class BoundaryMulti {
    multi method inner(Int $x) { callsame() // 'Nil' }
    multi method inner(Str $x) { 'str' }
    multi method outer(Int $x) { self.inner($x) ~ '/' ~ (callsame() // 'Nil') }
    multi method outer(Any $x) { 'outer-any' }
}
is BoundaryMulti.new.outer(1), 'Nil/outer-any',
    'an exhausted inner method preserves the enclosing method dispatcher';

sub boundary-helper() { callsame }
multi sub boundary-helper-outer(Int $x) { boundary-helper() }
multi sub boundary-helper-outer(Any $x) { 'any' }
is boundary-helper-outer(1), 'any', 'a plain sub still sees its caller dispatcher';

class BoundaryTerminal {
    method only() { callsame() // 'Nil' }
}
is BoundaryTerminal.new.only(), 'Nil', 'a sole method has an empty deferral boundary';
