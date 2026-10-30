use Test;

plan 6;

# `constant %h = <a Hash>` keeps the Hash (only a list is coerced to a Map),
# so its elements can still be stored and bound.
my constant %empty = hash;
%empty<a> = 1;
is %empty.raku, '{:a(1)}', 'constant %h = hash; element assignment';

my constant %lit = %(a => 1);
%lit<b> = 2;
is %lit.sort.map(*.key).join, 'ab', 'constant %h = %(...); element assignment';

my constant %bound := hash;
%bound<k> := 5;
is %bound<k>, 5, 'constant %h := hash; element binding';

my constant %map = (a => 1, b => 2);
dies-ok { %map<b> = 2 }, 'a Map constant stays immutable';
is %map.WHAT.gist, '(Map)', 'a list is still coerced to a Map';

class C {
    my constant %conf := hash;
    method ^configure(Mu $o, %config) { %conf{$o.WHAT.WHICH} := %config.Map }
    method ^configuration(Mu $o) { %conf{$o.WHAT.WHICH} // Map.new }
}
C.HOW.configure(C, %(x => 1));
is C.HOW.configuration(C).raku, 'Map.new((:x(1)))', 'a metamethod can fill a class-level constant hash';
