use Test;

# A `sub` declared inside a method body compiles to bytecode whose
# `RegisterSub` opcode's compiled-routine key must resolve against the
# *method's own* compiled-functions table, not an empty one substituted at
# the dispatch call site. Before this fix, that table was always empty, so
# the nested sub's plan bytecode never resolved and the method kept its AST
# body as a fallback (ADR-0019 C6e-3c "class-walker nested subs" keep-class).
# This still worked correctness-wise via the interpreter fallback; this file
# pins the behavior now that the method dispatch runs the compiled body with
# a real functions table.

plan 8;

class Basic {
    method double($x) {
        sub helper($y) { $y * 2 }
        helper($x);
    }
}
is Basic.new.double(10), 20, 'nested sub inside a method body';

class PerCall {
    method captured($n) {
        sub helper() { $n * 2 }
        helper();
    }
}
my $pc = PerCall.new;
is $pc.captured(5), 10, 'nested sub captures per-call value (first call)';
is $pc.captured(7), 14, 'nested sub captures per-call value (repeated call, idempotent re-registration)';

class Recursive {
    method fact($n) {
        sub fact-inner($k) { $k <= 1 ?? 1 !! $k * fact-inner($k - 1) }
        fact-inner($n);
    }
}
is Recursive.new.fact(5), 120, 'recursive nested sub inside a method body';

role Doubler {
    method qux($x) {
        sub double($y) { $y * 2 }
        double($x);
    }
}
class WithRole does Doubler { }
is WithRole.new.qux(7), 14, 'nested sub inside a role-composed method';

class MultiHost {
    multi method go(Int $x) {
        sub inc($v) { $v + 1 }
        inc($x);
    }
    multi method go(Str $x) {
        sub shout($v) { $v.uc }
        shout($x);
    }
}
my $mh = MultiHost.new;
is $mh.go(41), 42, 'nested sub inside one multi method candidate';
is $mh.go("hi"), 'HI', 'nested sub inside a different multi method candidate';

class Scoped {
    method secret-holder() {
        sub secret() { 42 }
        secret();
    }
}
is Scoped.new.secret-holder(), 42, 'nested sub is callable inside its method';
# Whether a method-nested sub leaks into the enclosing (global) scope is a
# separate, pre-existing bug (reproduces on main independent of this fix) —
# see todo/tickets/nested-sub-in-method-leaks-to-global-scope.md — so it is
# not asserted here.
