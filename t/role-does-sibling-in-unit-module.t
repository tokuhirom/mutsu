use v6;
use Test;

plan 6;

# Inside a module body, a role is registered under its qualified name
# (`M::Base`). A sibling role composing it by short name (`role Derived does
# Base`) must resolve through the module prefix — the class-body `does` path
# already did; the role-body path errored with "Unknown role: Base".
# JSON::Unmarshal composes all its CustomUnmarshaller roles this way
# (under `unit module JSON::Unmarshal`), which blocked `use JSON::Unmarshal`
# (and with it JSON::Marshal / Test::META).

module RoleSib {
    role Base {
        method b { "base" }
    }

    role Derived does Base {
        method d { "derived" }
    }

    role Third does Derived {
        method t { "third" }
    }

    class UsesThird does Third {
    }
}

my $x = RoleSib::UsesThird.new;
is $x.b, "base", 'grandparent role method via short-name chain';
is $x.d, "derived", 'parent role method via short-name chain';
is $x.t, "third", 'directly-composed role method';
ok $x ~~ RoleSib::Base, 'smartmatch against the qualified base role';
ok $x ~~ RoleSib::Derived, 'smartmatch against the qualified middle role';
ok $x ~~ RoleSib::Third, 'smartmatch against the qualified composed role';
