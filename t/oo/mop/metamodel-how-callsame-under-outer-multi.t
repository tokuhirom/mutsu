use Test;

# A user method on a subclass of a builtin metamodel HOW defers to the NATIVE
# metamethod with `callsame`. That base candidate is not a MethodDef, so the
# method once pushed no dispatch frame of its own, and its `callsame` resolved
# against whatever frame an enclosing routine had left live -- an outer
# `multi sub`'s, which answered with the wrong candidate. That is how
# `use-ok 'Terminal::ANSI'` (a `monitor` loaded from inside Test's
# `multi sub use-ok`) broke OO::Monitors' `MonitorHOW.new_type`.

plan 3;

class MyHOW is Metamodel::ClassHOW {
    method new_type(|) {
        my \type = callsame();
        type.HOW.^name
    }
}

is MyHOW.new_type(:name<Plain>), 'MyHOW', 'callsame reaches the native new_type';

multi sub outer($x) { MyHOW.new_type(:name<InMulti>) }
multi sub outer(Int $x) { 'int candidate' }

is outer('s'), 'MyHOW', 'an enclosing multi frame does not answer the HOW callsame';

multi sub via-eval($name) { EVAL 'MyHOW.new_type(:name<' ~ $name ~ '>)' }
multi sub via-eval(Int $x) { 'int candidate' }

is via-eval('Evaled'), 'MyHOW', 'same through EVAL inside a multi';
