use Test;

# `method ^name` is a method of the type's metaclass: another metamethod of
# the same type reaches it through `self`.
plan 2;

class A {
    method ^hello(Mu $obj is raw, $x) is raw { "hello $x from {$obj.^name}" }
    method ^viaself(|args) is raw { self.hello: |args }
}

is A.^hello(1), 'hello 1 from A', 'a metamethod called directly';
is A.^viaself(2), 'hello 2 from A', 'a metamethod called through self from another';
