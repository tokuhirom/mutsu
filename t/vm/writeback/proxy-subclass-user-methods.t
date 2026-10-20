use Test;
use nqp;

# ADR-14162 (AttrX::Mooish's AttrProxy): a `class is Proxy` subclass keeps
# its attributes and its own methods, public and private, on the value
# `.VAR` hands back; `callwith` in its `new` reaches `Proxy.new` and returns
# the Proxy itself, not what its FETCH answers.

plan 10;

my class AP is Proxy {
    has $.inst;
    has $!attr is built(:bind);
    has $!count = 0;
    method !SET-SELF(%attrinit) is raw {
        $!attr := %attrinit<attribute>;
        $!inst := %attrinit<instance>;
        self
    }
    method new(:$FETCH, :$STORE, *%attrinit) is raw {
        callwith(:$FETCH, :$STORE).VAR!SET-SELF(%attrinit)
    }
    method hello { "hello $!attr" }
    method bump { ++$!count }
    method count { $!count }
}

my $p := AP.new(
    FETCH => my sub ($proxy) is raw { 42 },
    STORE => my sub ($proxy, $value) is raw { },
    :attribute('A'), :instance('I'),
);

is $p, 42, 'reading the container runs FETCH';
ok nqp::istype_nd($p, AP), 'istype_nd sees the Proxy subclass without FETCHing';
is $p.VAR.hello, 'hello A', 'a public method sees the private attribute set by !SET-SELF';
is $p.VAR.inst, 'I', 'the public accessor';
is $p.VAR.bump, 1, 'a method that mutates an attribute';
is $p.VAR.bump, 2, 'the mutation persists between calls';
is $p.VAR.count, 2, 'and is read back';

# `nqp::decont` of a Proxy inside its own FETCH does not FETCH again.
my $depth = 0;
my $q := Proxy.new(
    FETCH => my sub ($proxy) is raw {
        $depth++;
        my $d := nqp::decont($proxy);
        7
    },
    STORE => my sub ($proxy, $value) is raw { },
);
is $q, 7, 'FETCH may nqp::decont its own proxy';
ok $depth < 4, 'without re-entering FETCH';

# A `Mu` raw parameter takes the Proxy as it is.
sub takes-mu(Mu $x is raw) { $x.VAR.^name }
my $calls = 0;
my $r := Proxy.new(
    FETCH => my sub ($proxy) is raw { $calls++; 1 },
    STORE => my sub ($proxy, $value) is raw { },
);
takes-mu($r);
is $calls, 0, 'binding a Proxy to a Mu raw parameter does not FETCH';

done-testing;
