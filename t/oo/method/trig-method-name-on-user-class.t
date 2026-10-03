use Test;

# A class that declares its own `cos` (a grammar actions class with a
# `method cos($/)`, PDF::Grammar) is not asking for Numeric coercion: the
# user method wins, also when the call is made through `.parse`'s actions.

plan 2;

class A { method cos($x) { "cos:$x" } }
is A.new.cos(1), 'cos:1', 'a user method named cos is called directly';

grammar G { token TOP { <cos> }; token cos { a } }
class Actions {
    method cos($/) { make 'cos' => 5 }
    method TOP($/) { make $<cos>.ast }
}
is-deeply G.parse('a', :actions(Actions.new)).ast, (cos => 5), 'actions method named cos';
