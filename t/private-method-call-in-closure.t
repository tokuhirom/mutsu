use v6;
use Test;

# A private method call is resolved lexically: `self!secret` written inside
# class A stays legal when the block containing it is invoked by ANOTHER
# object's method. DBDish::Pg's StatementHandle BUILD calls `self!get-meta`
# inside a block passed to `$!parent.protect-connection`.
plan 3;

class Runner {
    method run(&code) { code() }
}

class Owner {
    has $.runner;
    has $.log = '';
    method !secret($x) { $!log ~= "secret($x)"; $x * 2 }
    method go {
        $.runner.run: { self!secret(21) };
    }
}

my $o = Owner.new(runner => Runner.new);
is $o.go, 42, 'private call inside a closure run by another class works';
is $o.log, 'secret(21)', 'the private method body ran';

# Calling a private method from genuinely-foreign code must still die
# (compile-time in Rakudo, runtime in mutsu — EVAL covers both).
dies-ok { EVAL q[class Outsider { method poke($victim) { $victim!secret(1) } }; Outsider.new.poke(Owner.new(runner => Runner.new))] },
    'a foreign class still cannot call the private method';

done-testing;
