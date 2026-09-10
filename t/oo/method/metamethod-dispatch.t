use v6;
use Test;

# User-defined metamethods (`method ^foo(Mu) {...}`) dispatch through a
# dedicated fallback in `call_method_with_values`
# (`src/runtime/methods_call_dispatch.rs`, the "generalcalldispatch" site
# guarded by `method.starts_with('^')`). The VM prepends the type object as
# a plain leading positional argument (NOT bound as `self`/invocant) before
# reaching this fallback -- confirmed against `raku`. This file pins that
# calling convention, since it previously had zero coverage in the local
# `t/` corpus (see ADR-0019 F6's general-call-dispatch progress notes).

plan 6;

class Base {
    method ^describe(Mu \type) {
        "I am {type.^name}";
    }
}

is Base.^describe, 'I am Base', 'metamethod called on the declaring type object';

class Derived is Base {}

is Derived.^describe, 'I am Derived', 'inherited metamethod sees the calling subtype';

class WithArgs {
    method ^combine(Mu \type, *@rest) {
        "{type.^name}: " ~ @rest.join(',');
    }
}

is WithArgs.^combine(1, 2, 3), 'WithArgs: 1,2,3',
    'metamethod receives extra positional args after the type object';

my $inst = WithArgs.new;
is $inst.^combine('a', 'b'), 'WithArgs: a,b',
    'metamethod is callable through an instance, not just the type object';

class Counter {
    my $calls = 0;
    method ^bump(Mu \type) {
        $calls++;
        $calls;
    }
}

is Counter.^bump, 1, 'metamethod body executes (first call)';
is Counter.^bump, 2, 'metamethod body executes and retains state (second call)';

done-testing;
