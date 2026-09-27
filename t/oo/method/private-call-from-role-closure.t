use Test;

# Private-method access is lexical. A closure written in a role method keeps
# access to the role's private methods however it is later invoked — here a
# Proxy FETCH run from another class's `ACCEPTS` during `~~`. mutsu took the
# caller from the running method (`M::St`) and died with "Calling private
# method 'state' must be fully qualified". Found through Tinky, whose
# `State.ACCEPTS(Object)` reads the object's `state` Proxy.

plan 4;

module M {
    class St {
        has $.name;
        multi method ACCEPTS(St:D $s --> Bool) { self.name eq $s.name }
        multi method ACCEPTS(Obj:D $o --> Bool) { self ~~ $o.state }
    }
    role Obj {
        has St $.state;
        method !state( --> St ) is rw { $!state }
        method state(Obj:D $SELF:) is rw {
            Proxy.new(
                FETCH => method () { $SELF!state },
                STORE => method (St $val) { $SELF!state = $val },
            );
        }
    }
}

class Foo does M::Obj { }
my $a = M::St.new(name => 'a');
my $o = Foo.new(state => $a);
is $o.state.name, 'a', 'the Proxy reads through the private method';
ok $o ~~ $a, 'the matcher reads the Proxy from its own ACCEPTS';
nok $o ~~ M::St.new(name => 'b'), 'and compares the fetched value';
throws-like { Foo.new(state => $a)!M::Obj::state }, X::Method::Private::Permission,
    'an outside qualified call is still refused';
