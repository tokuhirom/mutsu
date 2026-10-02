use Test;

# A `No such method` raised INSIDE a grammar action's body is a real error and
# must propagate out of `.parse`, as in rakudo. Only a missing action method
# itself (no `method foo` for `token foo`) is silently skipped. mutsu#10703.

plan 5;

grammar G { token TOP { 'a' } }

class HyperMade { method TOP($/) { my @x = (1, 2)».made; make 1 } }
throws-like { G.parse('a', :actions(HyperMade)) }, X::Method::NotFound,
    method => 'made', typename => 'Int',
    'a missing method on a hyper call inside an action propagates';

class DirectMade { method TOP($/) { my $x = 1.made; make 1 } }
throws-like { G.parse('a', :actions(DirectMade.new)) }, X::Method::NotFound,
    method => 'made',
    'a missing method called directly inside an action propagates';

class Fine { method TOP($/) { make 42 } }
is G.parse('a', :actions(Fine)).made, 42, 'a working action still makes its value';

grammar H { proto token t {*}; token t:sym<a> { 'a' }; token TOP { <t> } }
class NoLeafAction { method TOP($/) { make 'top:' ~ ($<t>.made // 'none') } }
is H.parse('a', :actions(NoLeafAction)).made, 'top:none',
    'a rule with no action method is still skipped silently';

class Empty { }
ok G.parse('a', :actions(Empty)).defined, 'an actions class with no methods at all is fine';
