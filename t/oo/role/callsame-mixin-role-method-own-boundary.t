use Test;

# A role method called on a mixin (`Base.new but Tagged`) is its own dispatch
# boundary: when the mixed-in object has no method of the same name, its
# `callsame` / `nextsame` / `nextcallee` find no next candidate. They must not
# reach the enclosing method's deferral frame and consume its candidates
# (#18363). Expected values are rakudo's.
plan 5;

role Tagged {
    method defer-out()  { callsame() // 'no next' }
    method defer-next() { nextsame(); 'not reached' }
    method callee()     { nextcallee().defined ?? 'callee' !! 'no callee' }
}
role Wraps { method outer() { 'wrapped ' ~ callsame() } }

class Base { method outer() { 'base' } }

class Child is Base {
    method outer() { my $o = Base.new but Tagged; ($o.defer-out, callsame()).join('|') }
}
class Child2 is Base {
    method outer() { my $o = Base.new but Tagged; ($o.defer-next // 'Nil', callsame()).join('|') }
}
class Child3 is Base {
    method outer() { my $o = Base.new but Tagged; ($o.callee, callsame()).join('|') }
}
class Child4 is Base {
    method outer() { my $o = Base.new but Wraps; ($o.outer, callsame()).join('|') }
}

is Child.new.outer, 'no next|base',
    'callsame in a mixed-in role method leaves the enclosing candidates alone';
is Child2.new.outer, 'Nil|base', '...and so does nextsame';
is Child3.new.outer, 'no callee|base', '...and nextcallee';
is Child4.new.outer, 'wrapped base|base',
    'a role method over a base method still defers to it';
is (Base.new but Tagged).defer-out, 'no next', 'outside any method: no next candidate';
