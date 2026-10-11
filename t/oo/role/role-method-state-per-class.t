use Test;

plan 6;

# A `state` variable in a role method is cloned per composing class.
role R {
    method name-once() { state $n = self.^name; $n }
    method count() { state $c = 0; ++$c }
}
class A does R { }
class B does R { }

is A.new.name-once, 'A', 'first class gets its own state';
is B.new.name-once, 'B', 'second class does not see the first class state';
is A.new.count, 1, 'counter starts at 1 in A';
is A.new.count, 2, 'counter persists across A instances';
is B.new.count, 1, 'B counter is independent of A';
is B.new.count, 2, 'B counter persists';
