use Test;
# From Terminal::UI: `$!screen.init(self)` with `method init(\ui)`.
plan 3;
class S {
  method !trap(\ui) { 'trapped' }
  method init(\ui) { self!trap(ui) }
  method who(\ui) { self.^name }
}
class U {
  method add { S.new.init(self) }
  method who { S.new.who(self) }
}
is U.new.add, 'trapped', 'private call on own self after sigilless param bound to caller self';
is U.new.who, 'S', 'self inside callee is the callee invocant';
is S.new.who(U.new), 'S', 'unrelated arg unaffected';
