use Test;

# From HTML::Canvas: `has HTML::Canvas::Path2D $.path is graphics
# handles(@PathAPI) .= new: :canvas(self);`. `handles @names` took a variable,
# but inside parentheses only names, pairs and word lists were understood.
plan 5;

class P { method a { 1 }; method b { 2 }; method c { 3 } }
constant @Names = <a b>;

class C1 { has P $.p handles(@Names) = P.new; }
is C1.new.a, 1, 'handles(@var) with an initializer';
is C1.new.b, 2, 'delegates every name in the array';

class C2 { has P $.p is rw handles(@Names) .= new; }
is C2.new.b, 2, 'with a trait before it and .= new after it';

class C3 { has P $.p handles @Names = P.new; }
is C3.new.a, 1, 'the unparenthesised spelling still works';

class C4 { has P $.p handles(<c>) = P.new; }
is C4.new.c, 3, 'and so does a word list in parentheses';

# vim: expandtab shiftwidth=4
