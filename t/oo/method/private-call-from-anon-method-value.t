use Test;

# Private-method access is lexical. An anonymous `method` written in a role or
# class keeps access to that package's private methods when another class's
# method calls it through its code value.

plan 2;

role R {
    has $.x = 5;
    method !p { $!x }
    method get(R:D $SELF:) { method () { $SELF!p } }
}
class C does R { }

class K {
    has $.y = 7;
    method !q { $!y }
    method get(K:D $SELF:) { method () { $SELF!q } }
}

class D {
    method run($m) { $m(self) }
}

is D.new.run(C.new.get), 5, 'an anonymous method from a role reaches the role\'s private method';
is D.new.run(K.new.get), 7, 'an anonymous method from a class reaches the class\'s private method';
