use Test;

# A bare block literal passed as a parametric-role argument must bind to a
# `&callable` type parameter as the Block itself. Evaluated as a statement a
# bare `{ ... }` is a block that executes immediately, so
# `role R[&f]; class A does R[{ 1 }]` used to see an Int (or die) and match no
# role candidate.

plan 6;

role RCall[&f] { method apply(|c) { f(|c) } }

class A1 does RCall[{ 42 }] { }
is A1.new.apply, 42, 'bare block binds to &f and is callable from the role';

class A2 does RCall[{ $^n * 2 }] { }
is A2.new.apply(21), 42, 'block with a placeholder param works as &f';

class A3 does RCall[{ .<id> // die "no id" }] { }
is A3.new.apply({ id => 7 }), 7, 'block reading its argument (a hash) works as &f';
dies-ok { A3.new.apply({}) }, 'the block still dies when its own logic dies';

# Non-block callable arguments keep working (regression guard).
sub named-sub { 99 }
class A4 does RCall[&named-sub] { }
is A4.new.apply, 99, '&sub-ref still binds to &f';

my &blk = { 100 };
class A5 does RCall[&blk] { }
is A5.new.apply, 100, 'a &-sigil variable still binds to &f';

done-testing;
